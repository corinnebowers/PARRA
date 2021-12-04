
## run_PARRA.R
## Corinne Bowers
## 11/22/2021

## This code runs through the component models of the PARRA framework and outputs results for
## each pinch point variable as an .Rdata file.

##### define simulation information ###############################################################
cat('initializing script...\n')

## start code timer
timer <- Sys.time()

## set seed for reproducibility
set.seed(2021)

## setup information
setwd('/home/groups/bakerjw/cbowers/PARRA/')
#setwd('D:/1-PARRA/')
source('_data/setup.R')

## load required packages
require(units)

## load necessary information
load('_data/aoi/aoi.Rdata')
load('_data/catalog/catalog.Rdata')
load('_data/buildings/buildings.Rdata')
load('_data/NHD/NHD.Rdata')
load('_data/foundations/foundations.Rdata')
load('_data/depthdamage/depthdamage.Rdata')

## format depth-damage information
hazus <- hazus %>%
  filter(Basement == 'Y') %>%
  mutate(depth_m = depth_m + 3/mft) %>%
  group_by(depth_m) %>%
  summarize(damage_min = Min(damage_pct),
            damage_mean = Mean(damage_pct),
            damage_max = Max(damage_pct))
beta <- data.frame(depth_m = (0:100)/10) %>%
  mutate(alpha = predict(lm(alpha ~ depth_m, data = wing2020),
                         newdata = data.frame(depth_m))) %>%
  mutate(beta = 1/predict(lm(1/beta ~ depth_m, data = wing2020),
                          newdata = data.frame(depth_m))) %>%
  mutate(mu = alpha/(alpha+beta))

## register parallel backend
num_cores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK"))

## load from file, if necessary
run.elevated <- TRUE


#### load pinch points from stochastic simulation #################################################
Sys.time() - timer; timer <- Sys.time()
cat('\nloading inundation and damage realizations from original stochastic simulation...\n')

## load inundation level for each building & simulation
load('_results/stochastic/checkpoints/INUN.Rdata')

## load original (unmitigated) damage for each building & simulation
load('_results/stochastic/checkpoints/DM.Rdata')
simulations <- attr(damage, 'sim')
damage.stochastic <- damage

## load original (unmitigated) average loss for each building
load('_results/stochastic/checkpoints/DV.Rdata')
loss.stochastic <- loss.group


#### add information to buildings dataframe #######################################################
Sys.time() - timer; timer <- Sys.time()
cat('\nattaching information to buildings dataframe...\n')
 
## calculate 100-year flood elevation, meters 
## (measured relative to the vertical datum of the DEM)
rp100 <- 
  raster('_scripts/5_INUN/fit_inundation/5b_run_bestfit/results/bestfit.max', 
           crs = projection(aoi))
buildings <- buildings %>% 
  mutate(raised_m = strip(terra::extract(rast(rp100), st_coordinates(.))))

## calculate horizontal distance to the Russian River, meters
## (used for household prioritization scheme)
buildings <- buildings %>%
  mutate(dist_m = russian %>% st_union %>% st_distance(buildings, .) %>% strip %>% drop_units)

## rename columms and rearrange
buildings <- buildings %>% 
  rename(GEOID = tract, value.sd = acs.sd) %>%
  arrange(dist_m)


#### generate damage & loss realizations for elevated buildings ###################################
Sys.time() - timer; timer <- Sys.time()
source('_scripts/6_DM/DM_elevated.R')
cat('\ngenerating damage & loss realizations for elevated buildings...\n')

if (run.elevated) {
  ## get elevated damage by building & by simulation
  cat('   generating damage values...\n')
  damage.elevated <- 
    generate_damage(
      inundation = inundation,
      buildings = buildings,
      foundations = nsi1.found,
      curve = c('hazus', 'beta'),
      hazus = hazus, beta = beta,
      probabilistic = TRUE,
      n.mitigate = 1e10,
      n.damage = 1)

  ## checkpoint
  save(damage.elevated, file = '_results/mitigated/checkpoints/DM_elevated.Rdata')

  ## get elevated losses by building
  cat('   generating loss values...\n')
  id.wet <- attr(damage.elevated, 'buildings') %>% as.data.frame %>% pull(id)
  buildings.wet <- buildings %>% 
    arrange(bldg) %>% 
    filter(bldg %in% id.wet) %>% 
    st_drop_geometry
  values <- map2_dbl(
    .x = buildings.wet$value, .y = buildings.wet$value.sd,
    ~rnorm(1, mean = .x, sd = .y)) %>% 
    cbind(0) %>% apply(1, max)
  loss.elevated <- damage.elevated[[1]] %>% 
    as.data.frame %>% 
    select(-n.inun, -n.damage, -bldg) %>% 
    sweep(1, values, '*') %>% 
    cbind(damage.elevated[[1]] %>% as.data.frame %>% select(n.inun, n.damage, bldg))

  ## checkpoint
  save(loss.elevated, file = '_results/mitigated/checkpoints/DV_elevated.Rdata')

} else { #load from file
  load('_results/mitigated/checkpoints/DM_elevated.Rdata')
  load('_results/mitigated/checkpoints/DV_elevated.Rdata')
}


#### combine building-level loss estimates for original and elevated scenarios ###################
Sys.time() - timer; timer <- Sys.time()
cat('\ncreating a combined dataframe of original + elevated losses...\n')

## combine into one dataframe
loss.bldg <- 
  full_join(
    loss.stochastic %>% 
      rename(bldg = group, loss.stochastic = loss),
    loss.elevated %>% 
      select(-n.damage, -n.inun, -bldg) %>% 
      apply(1, mean) %>% 
      cbind(loss.elevated = ., bldg = loss.elevated$bldg) %>% 
      as.data.frame,
    by = 'bldg') %>% 
  left_join(
    buildings %>% st_drop_geometry %>% select(bldg, value, dist_m), 
    by = 'bldg') %>%
  arrange(dist_m) %>% 
  mutate(order = 1:nrow(.))


#### run quick loop to estimate where target will be met ##########################################
Sys.time() - timer; timer <- Sys.time()
cat('\ngenerating a ballpark estimate of number of buildings to mitigate...\n')

## NOTE: the total of losses by building is not the same as the AAL. Therefore this can only 
## be used as a starting point for a more complex calculation.

## iteratively elevate buildings & calculate new loss value
cl <- parallel::makeCluster(num_cores)
registerDoSNOW(cl)
pb <- txtProgressBar(min = 0, max = nrow(loss.bldg), style = 3)
loss.bldg$new_loss <- 
  foreach (
    i = 1:nrow(loss.bldg),
    .combine = 'c', .packages = 'tidyverse',
    .options.snow = list(progress = function(x) setTxtProgressBar(pb, x))) %dorng% {
      loss.bldg %>% 
        mutate(loss = case_when(order <= i ~ loss.elevated, TRUE ~ loss.stochastic)) %>% 
        pull(loss) %>% sum
    }
stopCluster(cl)
cat('\n')

## checkpoint
save(loss.bldg, file = '_results/mitigated/checkpoints/loss_bybldg.Rdata')


#### iteratively calculate mitigated AAL ##########################################################
Sys.time() - timer; timer <- Sys.time()
cat('\niteratively removing buildings and recalculating mitigated AAL...\n')

## define a starting point for mitigated search
n.bldg <- (loss.bldg$new_loss > (loss.bldg$new_loss[1]/2)) %>% sum

## arrange buildings by mitigation priority
buildings <- buildings %>% arrange(dist_m)

## iteratively calculate AAL
cl <- parallel::makeCluster(num_cores)
registerDoSNOW(cl)
pb <- txtProgressBar(min = 0, max = 500, style = 3)
temp <- 
  foreach (
#    i = (n.bldg-10):(n.bldg+10),
    i = c(1:500, seq(550, 1000, 50)),
    .packages = c('sf', 'pracma', 'tidyverse'),
    .export = c('generate_damage', 'generate_damage_probabilistic'),
    .options.snow = list(progress = function(x) setTxtProgressBar(pb, x))) %dorng% {
      ## get mitigated damage by building & by simulation
      damage.mitigated <- 
        generate_damage(
          inundation = inundation,
          buildings = buildings,
          foundations = nsi1.found,
          curve = c('hazus', 'beta'),
          hazus = hazus, beta = beta,
          probabilistic = TRUE,
          n.mitigate = i,
          n.damage = 1)

      ## get mitigated loss by simulation
      simulations <- attr(damage.mitigated, 'sim')
      id.wet <- attr(damage.mitigated, 'buildings') %>% as.data.frame %>% pull(id)
      buildings.wet <- buildings %>% 
        arrange(bldg) %>% 
        filter(bldg %in% id.wet) %>% 
        st_drop_geometry
      values <- map2_dbl(
        .x = buildings.wet$value, .y = buildings.wet$value.sd,
        ~rnorm(1, mean = .x, sd = .y)) %>% 
        cbind(0) %>% apply(1, max)
      loss.mitigated <- damage.mitigated[[1]] %>% 
        as.data.frame %>% 
        select(-n.inun, -n.damage, -bldg) %>% 
        sweep(1, values, '*') %>% 
        cbind(damage.mitigated[[1]] %>% as.data.frame %>% select(n.inun, n.damage, bldg))
      loss.sim <- loss.mitigated %>% 
        group_by(n.damage, n.inun) %>%
        summarize(across(1:(ncol(.)-3), sum), .groups = 'drop') %>% 
        setNames(c('n.damage', 'n.inun', 1:(ncol(.)-2))) %>% 
        pivot_longer(cols = -(1:2), names_to = 'sim', values_to = 'loss') %>% 
        transmute(sim = as.numeric(sim), loss) 

      ## save results
      save(loss.sim, file = paste0('_results/mitigated/results/DV_', i, '.Rdata'))
      NULL
    }
stopCluster(cl)
cat('\n')


#### save results #################################################################################
Sys.time() - timer; timer <- Sys.time()
cat('\ndone!\n\n')
