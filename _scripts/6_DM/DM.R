
## Note: there is no fit_damage function in this file. Rather than fitting an empirical 
## distribution between records of depth and damage, we used existing predefined depth-damage 
## relationships such as those from Hazus-MH and from Wing et al. (2020). 

###################################################################################################

generate_damage <- function(
  inundation, buildings, foundations, curve, hazus, beta, 
  probabilistic = FALSE, n.damage = 1) {
  #'
  #' Generates new realizations of damage ratios for every inundation realization and building 
  #' location.
  #'
  #' @param inun List of inundation heights (m) for every simulation and building location.
  #' Dimensions:
  #'   nrows = number of buildings that experience inundation
  #'   ncols = number of simulations that experience inundation
  #'   nlayers = n.inun
  #' Attributes: 
  #'   wet.bldg = indices of inundated buildings
  #'   buildings = tracker list of inundated buildings
  #'   sim = tracker list of probabilistic counters for inundated simulations
  #'   n.inun = number of inundation realizations to generate per hydrograph
  #' @param buildings sf dataframe (EPSG:6417) of building locations to generate 
  #' damage realizations for. Column "bldg" must be present.
  #' @param foundations Dataframe of foundation information for Sonoma County, 
  #' reported at the census tract level.
  #' @param curve Keyword that defines which depth-damage relationship to use. Valid keywords
  #' are "hazus" (Hazus-MH), "beta"/"wing" (Wing et al., 2020), or "average" (both).
  #' @param hazus Dataframe of Hazus-MH depth-damage relationships.
  #' @param beta Dataframe of Wing et al. (2020) depth-damage relationships.
  #' @param probabilistic Logical binary that indicates whether to incorporate uncertainty into 
  #' new damage realizations.
  #' @param n.damage Number of damage realizations to generate per building and simulation.
  #' 
  #' @return a list of damage ratios for every inundation realization and building location.
  #' Dimensions:
  #'   nrows = number of buildings that experience inundation
  #'   ncols = number of simulations that experience inundation + n.inun, n.damage, bldg
  #'   nlayers = n.inun
  #' Attributes: 
  #'   buildings = tracker list of inundated buildings
  #'   sim = tracker list of probabilistic counters for inundated simulations
  #' 

  ## fix input parameters
  if (!probabilistic) n.damage <- 1
  
  ## filter bad requests
  if (!any(c('hazus', 'beta', 'wing', 'average') %in% curve)) {
    stop('Not a valid keyword for "curve".')
  }

  ## extract attribute information from inundation list
  buildings <- buildings %>% arrange(bldg)
  wet.bldg <- attr(inundation, 'wet.bldg')
  n.inun <- length(inundation)

  ## calculate flood depth = inundation height - foundation height
  cat('   calculating first floor water depth...\n')
  pb <- txtProgressBar(min = 0, max = n.inun, style = 3)
  depth <- 
    foreach(i = 1:n.inun, 
      .packages = c('dplyr', 'purrr', 'sf', 'stringr'),
      .export = c('toNumber', 'assign_foundations'),
      .options.snow = list(progress = function(n) setTxtProgressBar(pb, n))) %dorng% {
        if (probabilistic) {
          found_ht <- assign_foundations(buildings[wet.bldg,], foundations)/3.28084 #mft
        } else found_ht <- rep(0, length(wet.bldg))
        temp <- sweep(inundation[[i]], 1, found_ht, FUN = '-') %>% 
          apply(2, function(x) ifelse(x<0, 0, x)) %>% as.matrix
        
        ## make sure basements outside of the floodplain are not being flooded
        temp[] <- ifelse(temp[]>0 & inundation[[i]][]==0, 0, temp[])
        temp
      }
  cat('\n')
  
  ## define interpolation function for beta distribution lookup
  find_nearest <- function(x, vector) {
    map_dbl(.x = x, .f = ~which.min(abs(.x-vector)))
  }
  
  ## generate damage ratios
  cat('   converting depth to damage...\n')
  pb <- txtProgressBar(min = 0, max = n.inun, style = 3)
  damage <- 
    foreach (i = 1:n.inun,
      .packages = c('dplyr', 'purrr', 'pracma'),
      .export = c('generate_damage_deterministic', 'generate_damage_probabilistic',
                  'find_nearest', 'Min', 'Mean', 'Max'),
      .options.snow = list(progress = function(n) setTxtProgressBar(pb, n))) %dorng% {
        if (probabilistic) {
          map_dfr(
            .x = 1:n.damage, 
            .f = ~generate_damage_probabilistic(
              depth[[i]], curve, hazus, beta) %>% 
              mutate(n.inun = ifelse(is.na(attr(inundation, 'n.inun')), NA, i), 
                     n.damage = .x, 
                     bldg = attr(inundation, 'buildings')[,'id']))
        } else {
          generate_damage_deterministic(
            depth[[i]], curve, hazus, beta) %>% 
            mutate(n.inun = ifelse(is.na(attr(inundation, 'n.inun')), NA, i), 
                   n.damage = NA, 
                   bldg = attr(inundation, 'buildings')[,'id'])
        }
      }
  cat('\n')
  
  ## return damage list
  attributes(damage)$sim <- attributes(inundation)$sim
  attributes(damage)$buildings <- attributes(inundation)$buildings
  return(damage)
}


###################################################################################################

assign_foundations <- function(buildings, foundations) {
  #'
  #' Assigns a foundation height (m) to each building location of interest based on the 
  #' distribution of foundation heights in the census tract associated with that building.
  #' 
  #' @param buildings sf dataframe (EPSG:6417) of building locations to generate 
  #' foundation heights for. Column "bldg" must be present.
  #' @param foundations Dataframe of foundation information for Sonoma County, 
  #' reported at the census tract level.
  #' 
  #' @return a vector of foundation heights for the buildings of interest.
  #'  

  ## randomly assign foundation information to buildings
  found <- names(foundations)[-1]
  buildings <- buildings %>%
    mutate(found_type = as.character(NA), found_ht = as.numeric(NA))
  for (geoid in unique(buildings$GEOID)) {
    n <- buildings %>% filter(GEOID == geoid) %>% nrow
    buildings[buildings$GEOID == geoid, c('found_type', 'found_ht')] <-
      foundations %>%
      filter(GEOID == geoid) %>% .[,-1] %>%
      sample(x = found, size = n, prob = ., replace = TRUE) %>%
      str_split(pattern = '_') %>%
      do.call(rbind, .) %>% as.data.frame %>%
      set_names(c('found_type', 'found_ht')) %>%
      mutate(found_type = paste(found_type),
             found_ht = toNumber(found_ht)) %>%
      mutate(found_ht = found_ht - ifelse(found_type == 'Basement', 10, 0))
  }
  return(buildings$found_ht)
}


###################################################################################################

generate_damage_deterministic <- function(inun, curve, hazus, beta) {
  #' 
  #' Helper function to generate deterministic realizations of expected damage for every 
  #' inundation realization and building location.
  #' 
  #' @param inun Matrix of inundation realizations for inundation simulation #i.
  #' @param curve Keyword that defines which depth-damage relationship to use. Valid keywords
  #' are "hazus" (Hazus-MH), "beta"/"wing" (Wing et al., 2020), or "average" (both).
  #' 
  #' @return a matrix of damage realizations for inundation simulation #i.
  #' 
  
  ## collapse depth matrix to vector 
  dm <- matrix(0, nrow = nrow(inun), ncol = ncol(inun))
  inunval <- c(unname(unlist(inun)))
  
  ## calculate damages
  dm.hazus <- inunval[inunval>0] %>% 
    cbind(max(hazus$depth_m)) %>% apply(1, min) %>% 
    interp1(hazus$depth_m, hazus$damage_mean, .)
  dm.beta <- beta$mu[find_nearest(inunval[inunval>0], beta$depth_m)]
  
  ## report correct damages based on curve type
  dm.combined <- cbind(
    if (any(c('hazus', 'average') %in% curve)) dm.hazus else  NULL,
    if (any(c('beta', 'wing', 'average') %in% curve)) dm.beta else NULL
  ) %>% rowMeans 

  ## return dataframe
  dm[inunval>0] <- dm.combined
  return(data.frame(dm))
}


###################################################################################################

generate_damage_probabilistic <- function(inun, curve, hazus, beta) {
  #' 
  #' Helper function to generate probabilistic realizations of expected damage for every 
  #' inundation realization and building location.
  #' 
  #' @param inun Matrix of inundation realizations for inundation simulation #i.
  #' @param curve Keyword that defines which depth-damage relationship to use. Valid keywords
  #' are "hazus" (Hazus-MH), "beta"/"wing" (Wing et al., 2020), or "average" (both).
  #' 
  #' @return a matrix of damage realizations for inundation simulation #i.
  #' 
  
  ## collapse depth matrix to vector 
  dm <- matrix(0, nrow = nrow(inun), ncol = ncol(inun))
  inunval <- c(unname(unlist(inun)))
  
  ## calculate damages
  dm.hazus <- map2_dbl(
    .x = inunval[inunval>0] %>% 
      cbind(max(hazus$depth_m)) %>% apply(1, min) %>% 
      interp1(hazus$depth_m, hazus$damage_min, .),
    .y = inunval[inunval>0] %>% 
      cbind(max(hazus$depth_m)) %>% apply(1, min) %>% 
      interp1(hazus$depth_m, hazus$damage_max, .),
    .f = ~runif(1, .x, .y))
  dm.beta <- map2_dbl(
    .x = beta$alpha[find_nearest(inunval[inunval>0], beta$depth_m)],
    .y = beta$beta[find_nearest(inunval[inunval>0], beta$depth_m)],
    .f = ~rbeta(1, shape1 = .x, shape2 = .y))
    
  ## report correct damages based on curve type
  dm.combined <- cbind(
    if (any(c('hazus', 'average') %in% curve)) dm.hazus else  NULL,
    if (any(c('beta', 'wing', 'average') %in% curve)) dm.beta else NULL
    ) %>% apply(1, function(x) x[sample(1:length(x), 1)])

  ## return dataframe
  dm[inunval>0] <- dm.combined
  return(data.frame(dm))
}


###################################################################################################
