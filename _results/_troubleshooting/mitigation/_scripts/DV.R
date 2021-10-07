
###################################################################################################

## @param
## damage (list): synthetic damages by building
  ## dimensions: 
   # nrows = number of buildings that experience inundation
         # = nrows(inundation) * n.damage
   # ncols = number of simulations that experience inundation + n.inun, n.damage, bldg
         # = ncols(inundation) + 3
   # nlayers = n.inun
  ## attributes: 
   # sim = tracker list of probabilistic counters
   # buildings = tracker list of inundated buildings
## buildings (data.frame): valuation & grouping information for buildings of interest
 # cols = bldg, value, value.sd, group
## aggregate (keyword): determines which type of results aggregation to perform
  ## options:
   # "group" reports average losses across all simulations by some spatial grouping (e.g. CT, watershed)
   # "sim" reports total losses for each simulated realization
## probabilistic (logical): choice to incorporate uncertainty
## n.loss (integer): number of loss realizations to generate per damage ratio

## @return
## loss.total (data.frame): total aggregated losses
 # cols = 

generate_losses <- function(
  damage, buildings, aggregate, probabilistic = FALSE, n.loss = 1, se.mult = 2) {

  ## fix input parameters
  if (!probabilistic) n.loss <- 1
  
  ## grab simulation tracker
  simulations <- attr(damage, 'sim')
  
  ## subset valuations & groupings to inundated buildings only
  id.wet <- attr(damage, 'buildings') %>% as.data.frame %>% pull(id)
  buildings.wet <- buildings %>% arrange(bldg) %>% 
    filter(bldg %in% id.wet) %>% 
    st_drop_geometry
  
  if (n.loss > 1) {
    pb <- txtProgressBar(min = 0, max = n.loss, style = 3)
    loss.total <- 
      foreach (i = 1:n.loss,
        .combine = 'rbind', .packages = c('dplyr', 'purrr', 'tidyr'),
        .options.snow = list(progress = function(n) setTxtProgressBar(pb, n)),
        .export = c('generate_group_losses', 'generate_sim_losses', 'toNumber')) %dorng% {

          ## determine building values
          if (probabilistic) {
            values <- map2_dbl(
              .x = buildings.wet$value, .y = buildings.wet$value.sd*se.mult,
              .f = ~rnorm(1, mean = .x, sd = .y)) %>% 
              cbind(0) %>% apply(1, max)
          } else values <- buildings.wet$value
          
          ## calculate losses
          losses <- damage %>% 
            lapply(function(x) {
              x %>% as.data.frame %>% 
                select(-n.inun, -n.damage, -bldg) %>% 
                sweep(1, values, '*') %>% 
                cbind(x %>% as.data.frame %>% select(n.inun, n.damage, bldg))})
          
          ## aggregate results
          if (aggregate == 'group') {
            loss.agg <- generate_group_losses(losses, buildings.wet)
          } else if (aggregate == 'sim') {
            loss.agg <- generate_sim_losses(losses, simulations)
          } else {
            stop('Not a valid keyword for "aggregate".')
          }
          rm(losses)
          if (probabilistic) loss.agg$n.loss <- i
          loss.agg
        }
  } else {
    ## determine building values
    if (probabilistic) {
      values <- map2_dbl(
        .x = buildings.wet$value, .y = buildings.wet$value.sd*2,
        ~rnorm(1, mean = .x, sd = .y)) %>% 
        cbind(0) %>% apply(1, max)
    } else values <- buildings.wet$value

    ## calculate losses
    losses <- damage %>% 
      lapply(function(x) {
        x %>% as.data.frame %>% 
          select(-n.inun, -n.damage, -bldg) %>% 
          sweep(1, values, '*') %>% 
          cbind(x %>% as.data.frame %>% select(n.inun, n.damage, bldg))})
    
    ## aggregate results
    if (aggregate == 'group') {
      loss.total <- generate_group_losses(losses, buildings.wet)
    } else if (aggregate == 'sim') {
      loss.total <- generate_sim_losses(losses, simulations)
    } else {
      stop('Not a valid keyword for "aggregate".')
    }
    if (probabilistic) loss.total$n.loss <- 1
    rm(losses)
  }

  ## return results
  if (aggregate == 'group') {
    loss.total <- loss.total %>% 
      group_by(group) %>% 
      summarize(loss = mean(loss), .groups = 'drop')
  }
  return(loss.total)
}


###################################################################################################

## @param
## losses (list): raw (unaggregated) losses by building & simulation
 # dimensions: nrows = nrows(damage), ncols = ncols(damage), nlayers = n.inun
## buildings.wet (data.frame): table of inundated buildings
 # cols = group

## @return
## losses.group (data.frame): total losses, aggregated by group

generate_group_losses <- function(losses, buildings.wet) {
  ## collapse all simulations to building-level mean losses
  losses.bldg <- losses %>% 
    lapply(function(x) {
      x %>% 
        group_by(bldg) %>% 
        summarize(across(1:(ncol(.)-3), mean), .groups = 'drop') %>% 
        mutate(loss = apply(.[,-1], 1, mean)) %>% 
        select(bldg, loss)
    }) %>% 
    reduce(full_join, by = 'bldg') %>% 
    mutate(loss = apply(.[,-1], 1, mean)) %>% 
    select(bldg, loss)
  
  ## convert to grouped losses
  losses.group <- losses.bldg %>% 
    right_join(buildings.wet %>% select(bldg, group), by = 'bldg') %>% 
    group_by(group) %>% 
    summarize(loss = mean(loss)) %>% 
    mutate(n.loss = NA)
  
  ## return grouped losses
  return(losses.group)
}


###################################################################################################

## @param
## losses (list): raw (unaggregated) losses by building & simulation
## simulations (data.frame): table of simulation indices

## @return
## losses.sim (data.frame): total losses, aggregated by simulation

generate_sim_losses <- function(losses, simulations) {
  ## collapse all buildings to total losses by simulation
  losses.sim <- losses %>% 
    lapply(function(x) {
      x %>% 
        group_by(n.damage, n.inun) %>%
        summarize(across(1:(ncol(.)-3), sum), .groups = 'drop') %>% 
        setNames(c('n.damage', 'n.inun', 1:(ncol(.)-2))) %>% 
        pivot_longer(cols = -(1:2), names_to = 'sim', values_to = 'loss') %>% 
        mutate(sim = as.numeric(sim)) %>% 
        left_join(simulations %>% mutate(sim = 1:nrow(.)), by = 'sim') 
    }) %>% 
    do.call(rbind, .) %>% 
    mutate(n.loss = NA) %>% 
    select(n.AR, n.precip, n.hc, n.runoff, n.hydro, n.inun, n.damage, n.loss, loss) 
  return(losses.sim)
}

