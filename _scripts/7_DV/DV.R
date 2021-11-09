
## Note: there is no fit_losses function in this file. Rather than fitting an empirical 
## distribution between records of damage and loss, we assumed that building-level losses would 
## be the product of expected damage ratio times the building valuation from tax assessor records.

###################################################################################################

generate_losses <- function(
  damage, buildings, aggregate, probabilistic = FALSE, n.loss = 1) {
  #'
  #' Generates new realizations of total loss for every damage realization and building location.
  #'
  #' @param damage List of damage ratios for every inundation realization and building location.
  #' Dimensions:
  #'   nrows = number of buildings that experience inundation
  #'   ncols = number of simulations that experience inundation + n.inun, n.damage, bldg
  #'   nlayers = n.inun
  #' Attributes: 
  #'   buildings = tracker list of inundated buildings
  #'   sim = tracker list of probabilistic counters for inundated simulations
  #' @param buildings sf dataframe (EPSG:6417) of building locations to generate 
  #' loss realizations for. Columns "bldg", "value", and "value.sd" must be present. Column 
  #' "group" must be present if you are reporting losses by group (aggregate = "group").
  #' @param foundations Dataframe of foundation information for Sonoma County, 
  #' reported at the census tract level.
  #' @param aggregate Keyword that defines how to report loss results. Valid keywords are "group" 
  #' (reports average losses across all simulations by some spatial grouping such as census tracts 
  #' or watersheds) or "sim" (reports total losses for each simulation).
  #' @param probabilistic Logical binary that indicates whether to incorporate uncertainty into 
  #' new loss realizations.
  #' @param n.loss Number of loss realizations to generate per building and simulation.
  #' 
  #' @return a dataframe of loss realizations, aggregated by group (aggregate = "group") or 
  #' by simulation (aggregate = "sim").
  #' 
  
  ## fix input parameters
  if (!probabilistic) n.loss <- 1
  
  ## grab simulation tracker
  simulations <- attr(damage, 'sim')
  
  ## subset valuations & groupings to inundated buildings only
  id.wet <- attr(damage, 'buildings') %>% as.data.frame %>% pull(id)
  buildings.wet <- buildings %>% arrange(bldg) %>% 
    filter(bldg %in% id.wet) %>% 
    st_drop_geometry

  pb <- txtProgressBar(min = 0, max = n.loss, style = 3)
  loss.total <- 
    foreach (i = 1:n.loss,
      .combine = 'rbind',
      .packages = c('dplyr', 'purrr', 'tidyr'),
      .export = c('generate_group_losses', 'generate_sim_losses', 'toNumber'),
      .options.snow = list(progress = function(n) setTxtProgressBar(pb, n))) %dopar% {
  
        ## determine building values
        if (probabilistic) {
          values <- map2_dbl(
            .x = buildings.wet$value, .y = buildings.wet$value.sd,
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
          losses.agg <- generate_group_losses(losses, buildings.wet)
        } else if (aggregate == 'sim') {
          losses.agg <- generate_sim_losses(losses, simulations)
        } else {
          stop('Not a valid keyword for "aggregate".')
        }
        rm(losses)
        if (probabilistic) losses.agg$n.loss <- i
        losses.agg
    }
    cat('\n')

    ## return results
    if (aggregate == 'group') {
      loss.total <- loss.total %>% 
        group_by(group) %>% 
        summarize(loss = mean(loss), .groups = 'drop')
    }
    return(loss.total)
}


###################################################################################################

generate_group_losses <- function(losses, buildings.wet) {
  #' 
  #' Aggregates loss realizations to estimate expected losses by spatial group.
  #' 
  #' @param losses List of loss realizations by building and simulation for loss simulation #i.
  #' @param buildings.wet Dataframe of buildings that were inundated in at least one simulation.
  #' Columns "bldg" and "group" must be present.
  #' 
  #' @return A dataframe of losses aggregated by group. 
  #' 
  
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

generate_sim_losses <- function(losses, simulations) {
  #' 
  #' Aggregates loss realizations to estimate total losses for each simulation.
  #' 
  #' @param losses List of loss realizations by building and simulation for loss simulation #i.
  #' @param simulations Dataframe of probabilistic counters for indexing simulations. Columns
  #' "n.AR", "n.precip", "n.hc", "n.runoff", "n.hydro", "n.inun", "n.damage", and "n.loss"
  #' must be present.
  #' 
  #' @return A dataframe of losses aggregated by simulation index. 
  #' 
  
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


###################################################################################################
