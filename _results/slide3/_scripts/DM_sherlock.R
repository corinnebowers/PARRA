
###################################################################################################

## @param
## inundation (list): synthetic flood depths by building
  ## dimensions: 
   # nrows = number of buildings that experience inundation
   # ncols = number of simulations that experience inundation
   # nlayers = n.inun
  ## attributes:
   # wet.bldg = indices of inundated buildings
   # buildings = tracker list of inundated buildings
   # sim = tracker list of probabilistic counters
   # n.inun = number of inundation realizations to generate per hydrograph
## buildings (sf points): geographic information for buildings of interest
 # cols = bldg, GEOID
## foundations (data.frame): foundation information by Sonoma census tract
 # cols = GEOID, [found_type _ found_ht]
## curve (keyword): determines which depth-damage relationship to use
  ## options:
   # "hazus" uses the HAZUS-MH depth-damage functions
   # "flemo" uses the FLEMOps depth-damage functions
   # "beta" or "wing" uses the beta distribution curves from Wing et al. (2020)
   # "average" draws from/takes an average of all three
## hazus:
## flemo:
## beta: 
## probabilistic (logical): choice to incorporate uncertainty
## n.damage (integer): number of flood damages to generate per inundation event

## @return
## damage (list): synthetic damages by building
  ## dimensions: 
   # nrows = number of buildings that experience inundation
         # = nrows(inundation) * n.damage
   # ncols = number of simulations that experience inundation + n.inun, n.damage, bldg
         # = ncols(inundation) + 3
   # nlayers = n.inun
  ## attributes:
   # buildings = tracker list of inundated buildings
   # sim = tracker list of probabilistic counters

generate_damage <- function(
  inundation, buildings, foundations, curve, 
  hazus = hazus, flemo = flemo, beta = beta,
  probabilistic = FALSE, n.damage = 1) {
  
  ## fix input parameters
  if (!probabilistic) n.damage <- 1
  
  ## filter bad requests
  if (!any(c('hazus', 'flemo', 'beta', 'wing', 'average') %in% curve)) {
    stop('Not a valid keyword for "curve".')
  }

  ## extract inundation information
  buildings <- buildings %>% arrange(bldg)
  wet.bldg <- attr(inundation, 'wet.bldg')
  n.inun <- length(inundation)
  
  print('subtracting foundation heights to determine flood depths...')
  
  ## calculate flood depth = inundation height - foundation height
  pb <- txtProgressBar(min = 0, max = n.inun, style = 3)
  depth <- 
    foreach(i = 1:n.inun, 
      .packages = c('dplyr', 'purrr', 'sf', 'stringr'),
      .export = c('toNumber', 'assign_foundations'),
      .options.snow = list(progress = function(n) setTxtProgressBar(pb, n))) %dopar% {
        if (probabilistic) {
          found_ht <- assign_foundations(buildings[wet.bldg,], foundations)
          # found_ht <- unname(unlist(st_drop_geometry(buildings[wet.bldg, 'raised_m']))) + 1
        } else found_ht <- rep(0, length(wet.bldg))
        temp <- sweep(inundation[[i]], 1, found_ht, FUN = '-') %>% 
          apply(2, function(x) ifelse(x<0, 0, x)) %>% as.matrix
        
        ## make sure basements outside of the floodplain are not being flooded
        temp[] <- ifelse(temp[]>0 & inundation[[i]][]==0, 0, temp[])
        temp
      }
  cat('\n')
  
  print('converting flood depths to damage ratios...')
  
  beta <- data.frame(depth_m = (0:100)/10) %>%
    mutate(alpha = predict(lm(alpha ~ depth_m, data = wing2020),
                           newdata = data.frame(depth_m))) %>%
    mutate(beta = 1/predict(lm(1/beta ~ depth_m, data = wing2020),
                            newdata = data.frame(depth_m))) %>%
    mutate(mu = alpha/(alpha+beta))

  ## define interpolation function for beta distribution lookup
  find_nearest <- function(x, vector) {
    map_dbl(.x = x, .f = ~which.min(abs(.x-vector)))
  }
  
  ## generate damage ratios
  pb <- txtProgressBar(min = 0, max = n.inun, style = 3)
  damage <- 
    foreach (i = 1:n.inun,
      .packages = c('dplyr', 'purrr', 'pracma'),
      .export = c('generate_damage_deterministic', 'generate_damage_probabilistic',
                  'find_nearest', 'Min', 'Mean', 'Max'),
      .options.snow = list(progress = function(n) setTxtProgressBar(pb, n))) %dopar% {
        if (probabilistic) {
          map_dfr(
            .x = 1:n.damage, 
            .f = ~generate_damage_probabilistic(
              depth[[i]], curve, hazus, flemo, beta) %>% 
              mutate(n.inun = ifelse(is.na(attr(inundation, 'n.inun')), NA, i), 
                     n.damage = .x, 
                     bldg = attr(inundation, 'buildings')[,'id']))
        } else {
          generate_damage_deterministic(
            depth[[i]], curve, hazus, flemo, beta) %>% 
            mutate(n.inun = ifelse(is.na(attr(inundation, 'n.inun')), NA, i), 
                   n.damage = NA, 
                   bldg = attr(inundation, 'buildings')[,'id'])
        }
      }
  cat('\n')
  
  attributes(damage)$sim <- attributes(inundation)$sim
  attributes(damage)$buildings <- attributes(inundation)$buildings
  return(damage)
}


###################################################################################################

## @param
## buildings (sf points): geographic information for buildings of interest
 # cols = GEOID
## foundations (data.frame): foundation information by Sonoma census tract

## @return
## found_ht (vector): foundation heights for buildings of interest

## randomly assign foundation info to buildings
assign_foundations <- function(buildings, foundations) {
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

## @param
## inun (matrix): inundation realizations for inundation simulation #i
## curve (keyword): determines which depth-damage relationship to use
  ## options:
   # "hazus" uses the HAZUS-MH depth-damage functions
   # "flemo" uses the FLEMOps depth-damage functions
   # "beta" or "wing" uses the beta distribution curves from Wing et al. (2020)
   # "average" draws from/takes an average of all three

## @return
## dm (matrix): damage realizations for inundation simulation #i

generate_damage_deterministic <- function(inun, curve, hazus, flemo, beta) {
  
  ## collapse depth matrix to vector 
  dm <- matrix(0, nrow = nrow(inun), ncol = ncol(inun))
  inunval <- c(unname(unlist(inun)))
  
  ## calculate damages
  dm.hazus <- inunval[inunval>0] %>% 
    cbind(max(hazus$depth_m)) %>% apply(1, min) %>% 
    interp1(hazus$depth_m, hazus$damage_mean, .)
  dm.flemo <- inunval[inunval>0] %>% 
    cbind(max(flemo$depth_m)) %>% apply(1, min) %>% 
    interp1(flemo$depth_m, flemo$damage_mean, .)
  dm.beta <- beta$mu[find_nearest(inunval[inunval>0], beta$depth_m)]
  
  ## report correct damages based on curve type
  dm.combined <- cbind(
    if (any(c('hazus', 'average') %in% curve)) dm.hazus else  NULL,
    if (any(c('flemo', 'average') %in% curve)) dm.flemo else NULL,
    if (any(c('beta', 'wing', 'average') %in% curve)) dm.beta else NULL
  ) %>% rowMeans 

  ## return dataframe
  dm[inunval>0] <- dm.combined
  return(data.frame(dm))
}


###################################################################################################

## @param
## inun (matrix): inundation realizations for inundation simulation #i
## curve (keyword): determines which depth-damage relationship to use
  ## options:
   # "hazus" uses the HAZUS-MH depth-damage functions
   # "flemo" uses the FLEMOps depth-damage functions
   # "beta" or "wing" uses the beta distribution curves from Wing et al. (2020)
   # "average" draws from/takes an average of all three

## @return
## dm (matrix): damage realizations for inundation simulation #i

generate_damage_probabilistic <- function(inun, curve, hazus, flemo, beta) {

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
  dm.flemo <- map2_dbl(
    .x = inunval[inunval>0] %>% 
      cbind(max(flemo$depth_m)) %>% apply(1, min) %>% 
      interp1(flemo$depth_m, flemo$damage_min, .),
    .y = inunval[inunval>0] %>% 
      cbind(max(flemo$depth_m)) %>% apply(1, min) %>% 
      interp1(flemo$depth_m, flemo$damage_max, .),
    .f = ~runif(1, .x, .y))
  dm.beta <- map2_dbl(
    .x = beta$alpha[find_nearest(inunval[inunval>0], beta$depth_m)],
    .y = beta$beta[find_nearest(inunval[inunval>0], beta$depth_m)],
    .f = ~rbeta(1, shape1 = .x, shape2 = .y))
    
  ## report correct damages based on curve type
  dm.combined <- cbind(
    if (any(c('hazus', 'average') %in% curve)) dm.hazus else  NULL,
    if (any(c('flemo', 'average') %in% curve)) dm.flemo else NULL,
    if (any(c('beta', 'wing', 'average') %in% curve)) dm.beta else NULL
    ) %>% rowMeans 

  ## return dataframe
  dm[inunval>0] <- dm.combined
  return(data.frame(dm))
}
