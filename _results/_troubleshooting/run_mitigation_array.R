###################################################################################################
print('initializing script...')

## set up R script
source('_data/setup.R')
source('_data/plots.R')

## load packages
require(units)

## define prioritization scheme
priority <- paste(commandArgs(trailingOnly=TRUE)[1])

## define job number
job <- toNumber(commandArgs(trailingOnly=TRUE)[2])

## load building information
load('_data/buildings/buildings.Rdata')
buildings <- buildings %>% rename(value.sd = acs.sd)

## load floodplain information
load('_data/NFHL/NFHL.Rdata')
buildings <- buildings %>% 
  mutate(NFHL = unname(unlist(terra::extract(obs, st_coordinates(buildings))))) %>% 
  arrange(desc(NFHL)) %>%
  mutate(id = 1:nrow(.)) %>% 
  group_by(NFHL) %>% 
  mutate(order = sample(id, length(id), replace = FALSE)) %>% 
  ungroup

## add distance to river
buildings <- 
  cbind(st_drop_geometry(buildings), st_coordinates(buildings)) %>% 
  st_as_sf(coords = c('X', 'Y'), crs = 6417)
buildings$dist_m <- buildings %>% 
  st_distance(russian %>% st_transform(6417) %>% st_union) %>% 
  c %>% drop_units


###################################################################################################
print('creating a combined dataframe of original + mitigated losses...')

## get original losses by building & by simulation
load(paste0('_results/stochastic/DM.Rdata'))
id.wet <- attr(damage, 'buildings') %>% as.data.frame %>% pull(id)
buildings.wet <- buildings %>% arrange(bldg) %>% 
  filter(bldg %in% id.wet) %>% 
  st_drop_geometry
values <- map2_dbl(
  .x = buildings.wet$value, .y = buildings.wet$value.sd,
  ~rnorm(1, mean = .x, sd = .y)) %>% 
  cbind(0) %>% apply(1, max)
loss.stochastic <- damage %>% 
  lapply(function(x) {
    x %>% as.data.frame %>% 
      select(-n.inun, -n.damage, -bldg) %>% 
      sweep(1, values, '*') %>% 
      cbind(x %>% as.data.frame %>% select(n.inun, n.damage, bldg))})

## get mitigated losses by building & by simulation
load(paste0('_results/mitigated/DM.Rdata'))
id.wet <- attr(damage, 'buildings') %>% as.data.frame %>% pull(id)
buildings.wet <- buildings %>% arrange(bldg) %>% 
  filter(bldg %in% id.wet) %>% 
  st_drop_geometry
values <- map2_dbl(
  .x = buildings.wet$value, .y = buildings.wet$value.sd,
  ~rnorm(1, mean = .x, sd = .y)) %>% 
  cbind(0) %>% apply(1, max)
loss.mitigated <- damage %>% 
  lapply(function(x) {
    x %>% as.data.frame %>% 
      select(-n.inun, -n.damage, -bldg) %>% 
      sweep(1, values, '*') %>% 
      cbind(x %>% as.data.frame %>% select(n.inun, n.damage, bldg))})

## combine into one dataframe
loss.bldg <- 
  left_join(
    loss.stochastic[[1]] %>% 
      select(-n.damage, -n.inun, -bldg) %>% 
      apply(1, mean) %>% 
      cbind(loss.stochastic = ., bldg = loss.stochastic[[1]]$bldg) %>% 
      as.data.frame,
    loss.mitigated[[1]] %>% 
      select(-n.damage, -n.inun, -bldg) %>% 
      apply(1, mean) %>% 
      cbind(loss.mitigated = ., bldg = loss.mitigated[[1]]$bldg) %>% 
      as.data.frame,
    by = 'bldg') %>% 
  left_join(
    buildings %>% st_drop_geometry %>% 
      select(bldg, value, dist_m, order), by = 'bldg')


###################################################################################################
print('arranging by prioritization scheme...')

## choose prioritization method
if (priority == 'loss') {		#arrange by expected loss
  loss.bldg <- loss.bldg %>% arrange(desc(loss.stochastic))
} else if (priority == 'value') {	#arrange by valuation
  loss.bldg <- loss.bldg %>% arrange(desc(value))
} else if (priority == 'random') {	#sort by NFHL status, then choose randomly
  loss.bldg <- loss.bldg %>% arrange(order)
} else if (priority == 'dist') {	#arrange by distance to the river
  loss.bldg <- loss.bldg %>% arrange(desc(dist_m))
} else {
  stop('Not a valid keyword for "priority".')
}


###################################################################################################
print('iteratively removing buildings and recalculating AAL...')

## grab simulation tracker
simulations <- attr(damage, 'sim')

## register parallel backend
num_cores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK"))

## elevate buildings iteratively & calculate new AAL
cl <- parallel::makeCluster(num_cores)
registerDoSNOW(cl)
pb <- txtProgressBar(min = 0, max = nrow(loss.bldg), style = 3)
loss.bldg$new_AAL <-
  foreach(
    i = 1:nrow(loss.bldg), 
    .combine = 'c', .packages = 'tidyverse', 
    .options.snow = list(progress = function(n) setTxtProgressBar(pb, n))) %dopar% {
      simulations$loss <- 
        rbind(
          loss.stochastic[[1]] %>% filter(!(bldg %in% loss.bldg$bldg[1:i])),
          loss.mitigated[[1]] %>% filter(bldg %in% loss.bldg$bldg[1:i])) %>% 
        select(-bldg, -n.damage, -n.inun) %>% 
        apply(2, sum)
      sum(simulations$loss)/3200
    }
stopCluster(cl)


###################################################################################################
print('saving out...')

## save out 
save(loss.bldg, file = paste0('_results/random/loss_bldg_', priority, '_', job, '.Rdata'))
print('done!')
