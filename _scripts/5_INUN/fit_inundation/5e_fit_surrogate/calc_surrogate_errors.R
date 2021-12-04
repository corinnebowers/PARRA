## surrogatemodel_array.R
## Corinne Bowers
## 7/22/2021

## This script runs cross-validation for choosing the tuning parameters n, p, and alpha for the IDW surrogate model.


#### setup information ############################################################################
print('defining input information...')

## setup information
setwd('/home/groups/bakerjw/cbowers/PARRA/')
#setwd('D:/1-PARRA/')
source('_data/setup.R')

## parallel backend
num.cores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK"))
#num_cores <- 5

## define job number
job <- toNumber(commandArgs(trailingOnly=TRUE)[1])

## load necessary information
load('_data/nonzero/nonzero.Rdata') #contains nonzero, nonzero.buffer, & points
load('_scripts/5_INUN/fit_inundation/5d_populate_grid/samples_grid.Rdata')

## set working folder
setwd('_scripts/5_INUN/fit_inundation/5e_fit_surrogate/')


### define user input information #################################################################

## define hyperparameter levels to consider
n.list <- c(1:6, 8, 10, 12, 15, 20)
p.list <- c(seq(0, 1, 0.25), 1.5, 2, 2.5, 3:5)
alpha.list <- seq(0.1, 0.9, 0.05)


#### define IDW surrogate function ################################################################

surrogate <- function(dist.matrix, i, n, p) {
  ## get the closest maps + associated weights
  distances <-
    data.frame(id = train.id, sim = train.sim,
               dist = dist.matrix[train.id, test.id[i]]) %>% arrange(dist)
  nearest <- distances[1:n, 'sim']
  weights <- 1/(distances[1:n, 'dist']^p)
  weights <- weights/sum(weights)
  
  ## find the prediction based on weighted average
  files <- paste0('../5d_populate_grid/results/max/grid', nearest, '.max')
  sim <- foreach(ii = 1:n, .combine = '+') %do% (raster(files[ii])*weights[ii])
  return(sim)
}


#### prepare samples table for cross-validation ###################################################
print('setting up train & test data for cross-validation...')

## define number of samples in grid
N <- nrow(samples)

## split data into train & test
good <- samples$sim[!is.na(samples$cv)]
cv <- samples$cv[good]
train.id <- (1:length(good))[cv!=job]
test.id <- (1:length(good))[cv==job]
train.sim <- good[train.id]
test.sim <- good[test.id]

## convert sample values to standard normal
samples_scale <- samples %>% 
  filter(sim %in% good) %>% arrange(sim) %>% 
  mutate(Qp.std = Qp %>% punif(min = 0, max = 4000) %>% 
           qunif(min = 1, max = exp(2)) %>% log,
         Qp.norm = qnorm(Qp.std/2)) %>% 
  mutate(tp.std = tp %>% punif(min = 0, max = 240) %>% 
           qunif(min = 1, max = exp(2)) %>% log,
         tp.norm = qnorm(tp.std/2))


###################################################################################################
print('calculating accuracy on withheld data...')

## find relevant cells for 50% inundation threshold and 1% inundation threshold
points50 <- which((nonzero >= (0.5*N))[])
points01 <- which((nonzero >= (0.01*N))[])

## run loop
start <- Sys.time()
pb <- txtProgressBar(min = 0, 
        max = length(n.list)*length(p.list)*length(alpha.list)*length(test.id), style = 3)
cl <- parallel::makeCluster(num.cores)
registerDoSNOW(cl)
error <-
  foreach (n = n.list, .combine = 'rbind', 
    .export = c('Sum', 'Mean', 'surrogate'),
    .options.snow = list(progress = function(n) setTxtProgressBar(pb, n)),
    .packages = c('raster', 'dplyr', 'tidyr', 'foreach')) %:% 
  foreach (p = p.list, .combine = 'rbind') %:%
  foreach (alpha = alpha.list, .combine = 'rbind') %:%
  foreach (i = 1:length(test.id), .combine = 'rbind') %dopar% {
      ## calculate observed map
      obs <- paste0('../5d_populate_grid/results/max/grid', test.sim[i], '.max') %>% raster
      
      ## calculate predicted raster
      dist.matrix <- samples_scale %>% 
        transmute(Qp = Qp.norm*alpha, tp = tp.norm*(1-alpha)) %>% 
        dist(method = 'euclidean') %>% as.matrix
      sim <- surrogate(dist.matrix, i, n = n, p = p)

      ## return results as dataframe
      pts50 <- (sim-obs)[points50]
      pts01 <- (sim-obs)[points01]
      data.frame(
        sim = test.sim[i], 
        n = n, p = p, alpha = alpha,
        SRSS50 = sqrt(Sum(pts50^2)),
        RMSE50 = sqrt(Mean(pts50^2)),
        MAE50 = Mean(abs(pts50)),
        SRSS01 = sqrt(Sum(pts01^2)),
        RMSE01 = sqrt(Mean(pts01^2)),
        MAE01 = Mean(abs(pts01)))
    }
stopCluster(cl)
Sys.time() - start

## save results
write.csv(error, 
  file = paste0('results/error', job, '.csv'))


###################################################################################################
