
##### define simulation information ###############################################################
print('initializing script...')

## load packages & custom functions
source('_data/setup.R')

## set random seed
set.seed(1)

## register parallel backend
num_cores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK"))

## read input information
args <- commandArgs(trailingOnly = TRUE)

## define number of Monte Carlo runs per model
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


##### define input information ####################################################################
print('loading location information...')

## define area of interest (aoi)
load('_data/aoi/aoi.Rdata')

## load dem
load('_data/lisflood/dem.Rdata')

## load historic catalog
load('_data/catalog/catalog.Rdata')


##### G(AR): generate AR realizations #############################################################
print('1. generate AR realizations')

## simulate new ARs
#source('_scripts/AR_sherlock.R')
#cl <- parallel::makeCluster(num_cores)
#registerDoSNOW(cl)
#AR <- 
#  generate_AR(
#    catalog = catalog, 
#    n.AR = n.ar, 
#    intensity.threshold = 0)
#stopCluster(cl)

## or, use a scenario event
AR <- catalog %>% 
  filter(start_day == '1995-01-06') %>% 
#   filter(start_day == '2005-12-30') %>% 
#   filter(start_day == '2019-02-25') %>% 
  transmute(n.AR = 1, IVT_max, duration, sm)

## checkpoint
save(AR, file = paste0(results, 'AR.Rdata'))


##### G(PRCP): generate precipitation realizations ################################################
print('2. generate precipitation and soil moisture realizations')
source('_scripts/PRCP_sherlock.R')

if (!is.na(prob.prcp) | !is.na(prob.hc)) {
  ## simulate precipitation values
  precip <- 
    generate_precip(
      AR = AR, 
      catalog = catalog,
      probabilistic = prob.prcp,
      n.precip = n.prcp)
  ## fit soil moisture distribution (and generate values, if necessary)
  precip <- generate_soilmoisture(precip, catalog)
  ## checkpoint
  save(precip, file = paste0(results, 'PRCP.Rdata'))
  
} else {
  ## load precipitation from file
  load(paste0(results, 'PRCP.Rdata'))
  
}
precip <- precip %>% filter(precip_mm > 0)


##### G(Q): generate runoff realizations #######################################################
print('3. generate runoff and hydrograph realizations')
source('_scripts/Q_sherlock.R')

if (!is.na(prob.rnff) | !is.na(prob.hydro)) {
  ## simulate runoff values
  runoff <- 
    generate_runoff(
      precip = precip, 
      catalog = catalog, 
      probabilistic = prob.rnff,
      n.runoff = n.rnff)
  ## convert runoff to a hydrograph (Qp+tp)
  hydrograph <- 
    generate_hydrograph(
      runoff = runoff, 
      catalog = catalog, 
      probabilistic = prob.hydro,
      n.hydro = n.hydro)
  ## checkpoint
  save(runoff, hydrograph, file = paste0(results, 'Q.Rdata'))
  
} else {
  ## load runoff & hydrograph information from file
  load(paste0(results, 'Q.Rdata'))
  
}
hydrograph <- hydrograph %>% filter(Qp_m3s > 0)


#### G(INUN): generate inundation realizations ####################################################
print('4. generate inundation realizations at building locations')
source('_scripts/INUN_sherlock.R')

## load building information
load('_data/buildings/buildings.Rdata')

## load sample information 
load('_data/samples.Rdata')  #copied from _sensitivity/surrogate

if (!is.na(prob.inun)) {
  ## calculate inundation height for each building
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


#### G(DM): generate damage realizations ##########################################################
print('5. generate damage realizations for each building & event')
source('_scripts/DM_sherlock.R')

## load foundation information
load('_data/foundations/foundations.Rdata')

## load depth-damage information
load('_data/depthdamage/depthdamage.Rdata')
hazus = hazus %>%
  filter(Basement == 'Y') %>%
  mutate(depth_m = depth_m + 3/mft) %>%
  group_by(depth_m) %>%
  summarize(damage_min = Min(damage_pct),
            damage_mean = Mean(damage_pct),
            damage_max = Max(damage_pct))
flemo = flemo %>% group_by(depth_m) %>%
  summarize(damage_min = Min(damage_pct), 
            damage_mean = Mean(damage_pct), 
            damage_max = Max(damage_pct))
beta = wing2020 %>% rbind(c(depth_ft = 0, alpha = 0, beta = 1, mu = 0, depth_m = 0))

if (!is.na(prob.dmg)) {
  ## calculate damage ratios for each building
  cl <- parallel::makeCluster(num_cores)
  registerDoSNOW(cl)
  damage <- 
    generate_damage(
      inundation = inundation,
      buildings = buildings %>% rename(GEOID = tract),
      foundations = nsi1.found,
      curve = 'beta',
      hazus = hazus, flemo = flemo, beta = beta,
      probabilistic = prob.dmg,
      n.damage = n.dmg)
  stopCluster(cl)
  ## checkpoint
  print('saving damage checkpoint...')
  save(damage, file = paste0(results, 'DM.Rdata'))
  
} else {
  ## load damage ratios from file
  load(paste0(results, 'DM.Rdata'))
  
}


#### G(DV): generate loss realizations ############################################################
print('6. generate loss realizations for each building & event')
source('_scripts/DV_sherlock.R')

cl <- parallel::makeCluster(num_cores)
registerDoSNOW(cl)
print('aggregating loss by census tract...')
loss.group <- 
  generate_losses(
    damage = damage,
    buildings = buildings %>% rename(group = tract, value = acs_value, value.sd = acs_sd),
    aggregate = 'group',
    probabilistic = prob.loss,
    n.loss = n.loss)

print('aggregating loss by simulation id...')
loss.sim <- 
  generate_losses(
    damage = damage,
    buildings = buildings %>% rename(group = tract, value = acs_value, value.sd = acs_sd),
    aggregate = 'sim',
    probabilistic = prob.loss,
    n.loss = n.loss)
stopCluster(cl)

## checkpoint
save(loss.group, loss.sim, file = paste0(results, 'DV.Rdata'))


###################################################################################################
print('done!')
