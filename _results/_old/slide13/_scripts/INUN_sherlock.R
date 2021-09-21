
###################################################################################################

## @param
## hydrograph (data.frame): list of synthetic hydrographs
 # cols = n.AR, n.precip, n.hc, n.runoff, n.hydro, Qp_m3s, tp_hrs
## samples (data.frame): list of existing LISFLOOD runs and associated values
 # cols = sim, cv, Qp, tp
## buildings (sf points): geographic information for buildings of interest
 # cols = bldg; EPSG = 6417
## probabilistic (logical): choice to incorporate uncertainty
## n.inun (integer): number of inundation realizations to generate per hydrograph
## n (double): size of search neighborhood for IDW interpolator
## p (double): power function for IDW interpolator
## alpha (double): anisotropy correction factor for IDW interpolator

## @return
## inundation (list): list of inundation heights (meters) by building & simulation
  ## dimensions: 
   # nrows = number of buildings that experience inundation 
         # = wet.bldg ~ [0, nrow(buildings)]
   # ncols = number of simulations that experience inundation 
         # = wet.sim ~ [0, nrow(hydrograph)]
   # nlayers = n.inun
  ## attributes:
   # wet.bldg = indices of inundated buildings
   # buildings = tracker list of inundated buildings
   # sim = tracker list of probabilistic counters
   # n.inun = number of inundation realizations to generate per hydrograph

generate_inundation <- function(
  hydrograph, samples, buildings, probabilistic = FALSE, n.inun = 1, 
  n = 6, p = 2.5, alpha = 0.75) {

  ## fix input parameters
  if (!probabilistic) n.inun <- 1

  ## separate building coordinates from building info
  buildings.coord <- buildings %>% arrange(bldg) %>% st_coordinates
  
  ## rescale samples to standard normal
  samples_scaled <- samples %>% 
    filter(!is.na(cv)) %>% 
    mutate(Qp.std = Qp %>% punif(min = 0, max = 4000) %>% 
             qunif(min = 1, max = exp(2)) %>% log,
           Qp.norm = qnorm(Qp.std/2)) %>% 
    mutate(tp.std = tp %>% punif(min = 0, max = 240) %>% 
             qunif(min = 1, max = exp(2)) %>% log,
           tp.norm = qnorm(tp.std/2))

  print('calculating inundation depths...')
  
  ## calculate inundation depth at each building
  pb <- txtProgressBar(min = 0, max = n.inun*nrow(hydrograph), style = 3)
  inundation.list <-
    foreach (inun = 1:n.inun) %:%
    foreach (i = 1:nrow(hydrograph),
      .packages = c('raster', 'terra', 'dplyr', 'foreach', 'pracma'),
      .export = c('surrogate'),
      .combine = 'cbind',
      .options.snow = list(progress = function(n) setTxtProgressBar(pb, n))) %dopar% {
        surrogate(
          Qp = hydrograph$Qp_m3s[i],
          tp = hydrograph$tp_hrs[i],
          sample.table = samples_scaled,
          sample.loc = '/home/groups/bakerjw/cbowers/LISFLOOD/grid_final/results/max/',
          # sample.loc = '_sensitivity/surrogate/sherlock_grid/results/',
          n, p, alpha, probabilistic) %>%
          rast %>% terra::extract(buildings.coord) %>%
          unlist %>% unname
        }
  cat('\n')
  
  # for (inun in 1:n.inun) {
  #   for (i in 1:nrow(hydrograph)) {
  #     print(cbind(hydrograph[i, c('n.precip', 'Qp_m3s', 'tp_hrs')], i=i))
  #     surrogate(
  #       Qp = hydrograph$Qp_m3s[i], 
  #       tp = hydrograph$tp_hrs[i],
  #       sample.table = samples_scaled,
  #       sample.loc = '/home/groups/bakerjw/cbowers/LISFLOOD/grid_final/results/max/',
  #       # sample.loc = '_sensitivity/surrogate/sherlock_grid/results/',
  #       n, p, alpha, probabilistic) %>%
  #       rast %>% terra::extract(buildings.coord) %>%
  #       unlist %>% unname 
  #   }
  # }

  ## clean out buildings + simulations that never flood
  wet.bldg <- inundation.list %>%
    lapply(FUN = function(inun) apply(data.frame(inun), 1, function(x) sum(x)>0)) %>%
    do.call(cbind, .) %>% apply(1, all) %>% which
  wet.sim <- inundation.list %>%
    lapply(FUN = function(inun) apply(data.frame(inun), 2, function(x) sum(x)>0)) %>%
    do.call(cbind, .) %>% apply(1, all) %>% which
  inundation <- inundation.list %>%
    lapply(function(inun) data.frame(inun)[wet.bldg, wet.sim] %>% as.matrix)
  rm(inundation.list)

  ## attach building + simulation information
  attr(inundation, 'wet.bldg') <- wet.bldg
  attr(inundation, 'buildings') <- cbind(id = 1:nrow(buildings), buildings.coord)[wet.bldg,]
  attr(inundation, 'sim') <- hydrograph[wet.sim,] %>% select(starts_with('n'))
  attr(inundation, 'n.inun') <- ifelse(probabilistic, n.inun, NA)

  ## return inundation
  return(inundation)
}


###################################################################################################

## @param
## Qp (double): value of peak flow (m3/s) we want to calculate inundation for
## tp (double): value of time to peak flow (hrs) we want to calculate inundation for
## sample.table (data.frame): list of existing LISFLOOD runs and associated values
 # cols = sim, Qp, Qp.norm, tp, tp.norm
## sample.loc (string): file location of LISFLOOD inundation database
## n (double): size of search neighborhood for IDW interpolator
## p (double): power function for IDW interpolator
## alpha (double): anisotropy correction factor for IDW interpolator
## probabilistic (logical): choice to incorporate uncertainty

## @return
## prediction (raster): inundation map predicted by IDW interpolator

surrogate <- function(Qp, tp, sample.table, sample.loc, n, p, alpha, probabilistic) {

  ## perform normal score transformation on new point
  Qp.new <- interp1(sort(sample.table$Qp), sort(sample.table$Qp.norm), Qp)*alpha
  tp.new <- interp1(sort(sample.table$tp), sort(sample.table$tp.norm), tp)*(1-alpha)

  ## find distances to new point
  distances <- sample.table %>% 
    mutate(Qp.scale = Qp.norm*alpha, tp.scale = tp.norm*(1-alpha)) %>% 
    mutate(dist = sqrt((Qp.scale-Qp.new)^2 + (tp.scale-tp.new)^2)) %>% 
    select(sim, dist) %>% arrange(dist)

  if (distances[1,'dist'] == 0) {
    ## predict based on exact match
    prediction <- raster(paste0(sample.loc, 'gridded', distances[1,'sim'], '.max'))
    
  } else {
    ## find the nearest maps
    nearest <- distances[1:n, 'sim']
    
    ## calculate the distance weights of the nearest maps
    weights <- 1/(distances[1:n, 'dist']^p)
    weights <- weights/sum(weights)
    
    if (probabilistic) {
      ## choose a random map from weighted sampling
      random <- sample(nearest, 1, prob = weights)
      prediction <- raster(paste0(sample.loc, 'gridded', random, '.max'))      
    } else {
      ## calculate weighted average of all maps
      files <- paste0(sample.loc, 'gridded', nearest, '.max')
      prediction <- foreach (ii = 1:n, .combine = '+') %do% (raster(files[ii])*weights[ii])
    }
  }
  return(prediction)
}



