
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
source('_data/setup.R')

## load necessary information
load('_data/aoi/aoi.Rdata')
load('_data/lisflood/dem.Rdata')
load('_data/catalog/catalog.Rdata')
load('_data/buildings/buildings.Rdata')
load('_scripts/5_INUN/fit_inundation/5d_populate_grid/samples_grid.Rdata')
load('_data/foundations/foundations.Rdata')
load('_data/depthdamage/depthdamage.Rdata')

## register parallel backend
num_cores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK"))

## read input information
args <- commandArgs(trailingOnly = TRUE)

## define number of Monte Carlo runs per variable
prob.ar <- as.logical(args[1]); n.ar <- as.numeric(args[2])
prob.prcp <- as.logical(args[3]); n.prcp <- as.numeric(args[4])
prob.hc <- as.logical(args[5]); n.hc <- as.numeric(args[6])
prob.rnff <- as.logical(args[7]); n.rnff <- as.numeric(args[8])
prob.hydro <- as.logical(args[9]); n.hydro <- as.numeric(args[10])
prob.inun <- as.logical(args[11]); n.inun <- as.numeric(args[12])
prob.dmg <- as.logical(args[13]); n.dmg <- as.numeric(args[14])
prob.loss <- as.logical(args[15]); n.loss <- as.numeric(args[16])

## set results folder
results <- args[17]


##### G(AR): generate AR realizations #############################################################
Sys.time() - timer; timer <- Sys.time()
cat('1. generate AR realizations\n')

## use the 2019 case study AR as a scenario event
AR <- catalog %>% 
  transmute(n.AR = 1:nrow(.), IVT_max, duration)

## checkpoint
save(AR, file = paste0(results, 'AR.Rdata'))


##### G(PRCP|AR): generate precipitation realizations #############################################
Sys.time() - timer; timer <- Sys.time()
cat('2. generate precipitation realizations\n')
source('_scripts/2_PRCP/PRCP.R')

if (!is.na(prob.prcp)) {
  ## fit precipitation component model
  fit_precip(catalog)

  ## simulate new precipitation values
  precip <- 
    generate_precip(
      AR = AR, 
      model.prcp, se.prcp,
      probabilistic = prob.prcp,
      n.precip = n.prcp)

  ## checkpoint
  save(precip, file = paste0(results, 'PRCP.Rdata'))
  
} else {
  ## load precipitation from file
  load(paste0(results, 'PRCP.Rdata'))
}


##### G(HC): generate soil moisture realizations ##################################################
Sys.time() - timer; timer <- Sys.time()
cat('3. generate soil moisture realizations\n')
source('_scripts/3_HC/HC.R')

if (!is.na(prob.hc)) {
  ## put antecedent hydrologic conditions component model
  fit_soilmoisture(catalog)

  ## simulate new soil moisture values (if necessary)
  # note: if the dataframe AR has a column named 'sm', those values will be used by default
  # instead of generating new values.
  precip.hc <- 
    generate_soilmoisture(
      precip = precip,
      fit.sm,
      probabilistic = prob.hc, 
      n.hc = n.hc)

  ## checkpoint
  save(precip.hc, file = paste0(results, 'HC.Rdata'))

} else {
  ## load soil moisture values from file
  load(paste0(results, 'HC.Rdata'))
}
precip.hc <- precip.hc %>% filter(precip_mm > 0)


##### G(Q|PRCP,HC): generate streamflow realizations ##############################################
Sys.time() - timer; timer <- Sys.time()
cat('4. generate streamflow realizations\n')
source('_scripts/4_Q/Q.R')

if (!is.na(prob.rnff) | !is.na(prob.hydro)) {
  ## fit runoff model & simulate new runoff values
  cat('   generating runoff values...\n')
  runoff <- 
    fit_generate_runoff(
      precip = precip.hc, 
      catalog = catalog, 
      probabilistic = prob.rnff,
      n.runoff = n.rnff)

  ## fit streamflow component model
  fit_Qp(catalog) #peak streamflow
  fit_tp(catalog) #time to peak streamflow
  
  ## simulate new streamflow values
  cat('   generating streamflow values...\n')
  hydrograph <- 
    generate_hydrograph(
      runoff = runoff, 
      model.Qp, se.Qp, fit.tp, 
      probabilistic = prob.hydro,
      n.hydro = n.hydro)

  ## checkpoint
  save(runoff, hydrograph, file = paste0(results, 'Q.Rdata'))
  
} else {
  ## load runoff & hydrograph information from file
  load(paste0(results, 'Q.Rdata'))
  
}
hydrograph <- hydrograph %>% filter(Qp_m3s > 0)


#### G(INUN|Q): generate inundation realizations ##################################################
Sys.time() - timer; timer <- Sys.time()
cat('5. generate inundation realizations for each building & event\n')
source('_scripts/5_INUN/generate_inundation/INUN.R')

if (!is.na(prob.inun)) {
  ## simulate new inundation values for each building and event
  cl <- parallel::makeCluster(num_cores)
  registerDoSNOW(cl)
  inundation <- 
    generate_inundation(
      hydrograph = hydrograph,
      samples = samples,
      buildings = buildings,
      probabilistic = prob.inun,
      n.inun = n.inun)
  stopCluster(cl)

  ## checkpoint
  save(inundation, file = paste0(results, 'INUN.Rdata'))
  
} else {
  ## load inundation from file
  load(paste0(results, 'INUN.Rdata'))
}


#### G(DM|INUN): generate damage realizations #####################################################
Sys.time() - timer; timer <- Sys.time()
cat('6. generate damage realizations for each building & event\n')
source('_scripts/6_DM/DM.R')

if (!is.na(prob.dmg)) {
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

  ## simulate new damage values for each building & event
  cl <- parallel::makeCluster(num_cores)
  registerDoSNOW(cl)
  damage <- 
    generate_damage(
      inundation = inundation,
      buildings = buildings %>% rename(GEOID = tract),
      foundations = nsi1.found,
      curve = c('hazus', 'beta'),
      hazus = hazus, beta = wing2020,
      probabilistic = prob.dmg,
      n.damage = n.dmg)
  stopCluster(cl)

  ## checkpoint
  save(damage, file = paste0(results, 'DM.Rdata'))
  
} else {
  ## load damage ratios from file
  load(paste0(results, 'DM.Rdata'))
  
}


#### G(DV|DM): generate loss realizations #########################################################
Sys.time() - timer; timer <- Sys.time()
cat('7. generate loss realizations for each building & event\n')
source('_scripts/7_DV/DV.R')

cl <- parallel::makeCluster(num_cores)
registerDoSNOW(cl)
cat('   aggregating loss by building...\n')
loss.group <- 
  generate_losses(
    damage = damage,
    buildings = buildings %>% mutate(group = bldg, value.sd = acs.sd),
    aggregate = 'group',
    probabilistic = prob.loss,
    n.loss = n.loss)

cat('   aggregating loss by simulation id...\n')
loss.sim <- 
  generate_losses(
    damage = damage,
    buildings = buildings %>% rename(group = blockgroup, value.sd = acs.sd),
    aggregate = 'sim',
    probabilistic = prob.loss,
    n.loss = n.loss)
stopCluster(cl)

## checkpoint
save(loss.group, loss.sim, file = paste0(results, 'DV.Rdata'))


###################################################################################################
Sys.time() - timer; timer <- Sys.time()
cat('done!\n\n')
