
###################################################################################################

## @param
## inundation (list): synthetic flood depths by building
  # attributes: sim = tracker list of n.AR, n.precip, & n.runoff
  #             buildings = tracker list of inundated buildings
  #             n.inun = number of inundation realizations to generate per hydrograph
## buildings (data.frame): geographic information for buildings of interest
## foundations (data.frame): foundation information by Sonoma CBG
## curve (keyword): "hazus" uses the HAZUS-MH depth-damage functions
##                  "flemo" uses the FLEMOps depth-damage functions
##                  "beta" uses the beta distribution curves from Wing et al. (2020)
##                  "average" draws from/takes an average of all three
## probabilistic (logical): choice to incorporate uncertainty
## n.damage (integer): number of flood damages to generate per inundation event

## @return
## damage (list): synthetic damages by building
  # attributes: sim = tracker list of n.AR, n.precip, & n.runoff
  #             buildings = tracker list of inundated buildings

generate_damage <- function(inundation, buildings, foundations, curve, 
                            probabilistic = FALSE, n.damage = 1) {
  
  ## fix input parameters
  if (!probabilistic) n.damage <- 1
  
  ## extract inundation information
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
        } else {
          found_ht <- rep(0, length(wet.bldg))
        }
        temp <- sweep(inundation[[i]], 1, found_ht, FUN = '-') %>% 
          apply(2, function(x) ifelse(x<0, 0, x)) %>% as.matrix
        temp[] <- ifelse(temp[]>0 & inundation[[i]][]==0, 0, temp[])
        temp
      }
  cat('\n')
  
  print('converting flood depths to damage ratios...')
  load('_data/depthdamage/depthdamage.Rdata')
  
  ## generate damage ratios
  pb <- txtProgressBar(min = 0, max = n.inun, style = 3)
  damage <- 
    foreach (i = 1:n.inun,
      .packages = c('dplyr', 'purrr', 'pracma'),
      .export = c('generate_damage_deterministic', 'generate_damage_probabilistic',
                  'Min', 'Mean', 'Max'),
      .options.snow = list(progress = function(n) setTxtProgressBar(pb, n))) %dopar% {
        if (probabilistic) {
          map_dfr(
            .x = 1:n.damage, 
            .f = ~generate_damage_probabilistic(
              depth[[i]], curve, hazus, flemo, wing2020) %>% 
              mutate(n.inun = ifelse(is.na(attr(inundation, 'n.inun')), NA, i), 
                     n.damage = .x, 
                     bldg = attr(inundation, 'buildings')[,'id']))
        } else {
          generate_damage_deterministic(
            depth[[i]], curve, hazus, flemo, beta.dist) %>% 
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
## buildings (data.frame): geographic information for buildings of interest
## foundations (data.frame): foundation information by Sonoma CBG

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
      foundations %>% filter(GEOID == geoid) %>% .[,-1] %>% 
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
## curve (keyword): "hazus" uses the HAZUS-MH depth-damage functions
##                  "flemo" uses the FLEMOps depth-damage functions
##                  "beta" uses the beta distribution curves from Wing et al. (2020)
##                  "average" draws from/takes an average of all three

## @return
## dm (matrix): damage realizations for inundation simulation #i

generate_damage_deterministic <- function(inun, curve, hazus, flemo, beta.dist) {
  dm <- matrix(0, nrow = nrow(inun), ncol = ncol(inun))
  inunval <- c(unname(unlist(inun)))
  inunval <- ifelse(inunval > 50, 50, inunval)
  inunval_clean <- inunval[!is.na(inunval) & round(inunval) > 0]
  
  if (curve == 'hazus') {
    dm[inunval > 0] <- interp1(hazus$ft, hazus$x1, inunval[inunval>0])/100
  } else if (curve == 'flemo') {
    dm[inunval > 0] <- interp1(flemo$ft, flemo$PQ_SFH, inunval[inunval>0])/100
  } else if (curve == 'beta') {
    dm[!is.na(inunval) & round(inunval) > 0] <- 
      interp1(beta.dist$water_ft, beta.dist$mu, inunval_clean)
  } else if (curve == 'average') {
    dm[!is.na(inunval) & round(inunval) > 0] <- 
      cbind(interp1(hazus$ft, hazus$x1, inunval_clean)/100, 
            interp1(flemo$ft, flemo$PQ_SFH, inunval_clean)/100, 
            interp1(beta.dist$water_ft, beta.dist$mu, inunval_clean)) %>% 
      rowMeans
  } else {
    stop('Not a valid keyword for "curve".')
  }
  
  dm <- data.frame(dm)
  return(dm)
}


###################################################################################################

## @param
## inun (matrix): inundation realizations for inundation simulation #i
## curve (keyword): "hazus" uses the HAZUS-MH depth-damage functions
##                  "flemo" uses the FLEMOps depth-damage functions
##                  "beta" uses the beta distribution curves from Wing et al. (2020)
##                  "average" draws from/takes an average of all three

## @return
## dm (matrix): damage realizations for inundation simulation #i

generate_damage_probabilistic <- function(inun, curve, hazus, flemo, wing2020) {
  dm <- matrix(0, nrow = nrow(inun), ncol = ncol(inun))
  inunval <- c(unname(unlist(inun)))
  inunval <- ifelse(inunval > 10, 10, inunval)
  inunval_clean <- inunval[!is.na(inunval) & round(inunval) > 0]
  
  if (curve == 'hazus') {
    hazus.temp <- hazus %>% 
      filter(Basement == 'N') %>% 
      group_by(depth_m) %>% 
      summarize(xmin = Min(damage_pct)/100, xmax = Max(damage_pct)/100) 
    hazus.temp <- hazus.temp %>% 
      rbind(c(depth_m = 10, 
              xmin = hazus.temp$xmin[nrow(hazus.temp)],
              xmax = hazus.temp$xmax[nrow(hazus.temp)]))
    dm[inunval > 0] <- 
      map2_dbl(.x = interp1(hazus.temp$depth_m, hazus.temp$xmin, inunval[inunval > 0]), 
               .y = interp1(hazus.temp$depth_m, hazus.temp$xmax, inunval[inunval > 0]), 
               .f = ~runif(1, .x, .y))
  } else if (curve == 'flemo') {
    dm[inunval > 0] <- 
      map2_dbl(.x = interp1(flemo$ft, flemo$HQ_SFH, inunval[inunval > 0])/100, 
               .y = interp1(flemo$ft, flemo$PQ_SFH, inunval[inunval > 0])/100, 
               .f = ~runif(1, .x, .y))
  } else if (curve == 'beta') {
    find_nearest <- function(x, vector) map_dbl(.x = x, .f = ~which.min(abs(.x-vector)))
    dm[!is.na(inunval) & round(inunval) > 0] <- 
      rbeta(n = length(inunval_clean), 
            shape1 = wing2020[find_nearest(inunval_clean, wing2020$depth_m), 'alpha'],
            shape2 = wing2020[find_nearest(inunval_clean, wing2020$depth_m), 'beta'])
    
  } else if (curve == 'average') {
    dm.hazus <- dm
    dm.hazus[inunval > 0] <- 
      map2_dbl(.x = interp1(hazus$ft, hazus$xmin, inunval[inunval > 0])/100, 
               .y = interp1(hazus$ft, hazus$xmax, inunval[inunval > 0])/100, 
               .f = ~runif(1, .x, .y))
    dm.flemo <- dm
    dm.flemo[inunval > 0] <- 
      map2_dbl(.x = interp1(flemo$ft, flemo$HQ_SFH, inunval[inunval > 0])/100, 
               .y = interp1(flemo$ft, flemo$PQ_SFH, inunval[inunval > 0])/100, 
               .f = ~runif(1, .x, .y))
    dm.beta <- dm
    dm.beta[!is.na(inunval) & round(inunval) > 0] <- 
      rbeta(n = length(inunval_clean), 
            shape1 = beta.dist[match(round(inunval_clean), beta.dist$water_ft), 'alpha'],
            shape2 = beta.dist[match(round(inunval_clean), beta.dist$water_ft), 'beta'])
    
    dm <- cbind(c(dm.hazus), c(dm.flemo), c(dm.beta)) %>% 
      apply(1, function(x) x[sample(1:3, 1)]) %>% 
      matrix(nrow = nrow(dm), ncol = ncol(dm))
    
  } else {
    stop('Not a valid keyword for "curve".')
  }
  
  dm <- data.frame(dm)
  return(dm)
}
