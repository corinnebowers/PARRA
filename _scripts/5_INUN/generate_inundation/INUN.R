
## Note: there is no fit_inundation function in this file because of the complex, multi-step 
## nature of the fit and calibration process for this particular component model. Instead, please 
## refer to the markdown files fit_lisflood.Rmd and fit_surrogate.Rmd for more information. 

###################################################################################################

generate_inundation <- function(
  hydrograph, samples, buildings, probabilistic = FALSE, n.inun = 1, 
  n = 6, p = 2.5, alpha = 0.75) {
  #'
  #' Generates new realizations of inundation at all specified buildings locations based on 
  #' the hydrograph parameterss and site conditions. 
  #' 
  #' @param hydrograph Dataframe of hydrograph parameters to generate inundations for. Columns 
  #' "n.AR", "n.precip", "n.hc", "n.runoff", "n.hydro", "Qp_m3s", and "tp_hrs" must be present.
  #' @param samples Dataframe of hydrograph parameters associated with pre-generated LISFLOOD 
  #' inundation maps. Used to define the sample space for the surrogate model. Columns "sim", 
  #' "cv", "Qp", and "tp" must be present.
  #' @param buildings sf dataframe (EPSG:6417) of building locations to generate 
  #' inundations for. Column "bldg" must be present.
  #' @param probabilistic Logical binary that indicates whether to incorporate uncertainty into 
  #' new inundation realizations.
  #' @param n.inun Number of inundation realizations to generate per hydrograph.
  #' @param n Size of search neighborhood for IDW interpolator.
  #' @param p Power function for IDW interpolator.
  #' @param alpha Anisotropy correction factor for IDW interpolator.
  #' 
  #' @return a list of inundation heights (m) for every simulation and building location.
  #' Dimensions:
  #'   nrows = number of buildings that experience inundation
  #'   ncols = number of simulations that experience inundation
  #'   nlayers = n.inun
  #' Attributes: 
  #'   wet.bldg = indices of inundated buildings
  #'   buildings = tracker list of inundated buildings
  #'   sim = tracker list of probabilistic counters for inundated simulations
  #'   n.inun = number of inundation realizations to generate per hydrograph
  
  ## fix input parameters
  if (!probabilistic) n.inun <- 1

  ## separate building coordinates from building information
  buildings.coord <- buildings %>% arrange(bldg) %>% st_coordinates
  
  ## rescale hydrograph parameters in sample space to standard normal
  samples_scaled <- samples %>% 
    filter(!is.na(cv)) %>% 
    mutate(Qp.std = Qp %>% punif(min = 0, max = 4000) %>% 
             qunif(min = 1, max = exp(2)) %>% log,
           Qp.norm = qnorm(Qp.std/2)) %>% 
    mutate(tp.std = tp %>% punif(min = 0, max = 240) %>% 
             qunif(min = 1, max = exp(2)) %>% log,
           tp.norm = qnorm(tp.std/2))

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
          # sample.loc = '/home/groups/bakerjw/cbowers/LISFLOOD/grid_final/results/max/',
          sample.loc = '_sensitivity/surrogate/sherlock_grid/results/',
          n, p, alpha, probabilistic) %>%
          rast %>% terra::extract(buildings.coord) %>%
          unlist %>% unname
        }
  cat('\n')

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

  ## attach building + simulation information to list
  attr(inundation, 'wet.bldg') <- wet.bldg
  attr(inundation, 'buildings') <- cbind(id = 1:nrow(buildings), buildings.coord)[wet.bldg,]
  attr(inundation, 'sim') <- hydrograph[wet.sim,] %>% select(starts_with('n'))
  attr(inundation, 'n.inun') <- ifelse(probabilistic, n.inun, NA)

  ## return inundation list
  return(inundation)
}


###################################################################################################

surrogate <- function(Qp, tp, sample.table, sample.loc, n, p, alpha, probabilistic) {
  #'
  #' Rapidly calculates inundation heights at all building locations. Uses inverse distance 
  #' weighted (IDW) interpolation over the sample space of pre-generated LISFLOOD maps as a 
  #' surrogate model to replace "expensive" hydrodynamic calculations. 
  #' 
  #' @param Qp Value of peak streamflow (m3/s) to calculate inundation for.
  #' @param tp Value of time to peak streamflow (hrs) to calculate inundation for.
  #' @param sample.table Dataframe of hydrograph parameters associated with pre-generated 
  #' LISFLOOD inundation maps, with hydrograph parameters in the sample space transformed
  #' to standard normal variates. Columns "sim", "Qp", "Qp.norm", "tp", and "tp.norm"
  #' must be present.
  #' @param sample.loc Text string indicating the location of the folder of 
  #' pre-generated LISFLOOD maps.
  #' @param n Size of search neighborhood for IDW interpolator.
  #' @param p Power function for IDW interpolator.
  #' @param alpha Anisotropy correction factor for IDW interpolator.
  #' @param probabilistic Logical binary that indicates whether to incorporate uncertainty into 
  #' new inundation realizations.
  #' 
  #' @return A raster map of inundation heights predicted by the IDW surrogate model.
  
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


###################################################################################################

