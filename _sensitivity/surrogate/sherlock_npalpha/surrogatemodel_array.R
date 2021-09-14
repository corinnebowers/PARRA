## surrogatemodel_array.R
## Corinne Bowers
## 7/22/2021

## This script runs cross-validation for choosing the tuning parameters n, p, and alpha for the IDW surrogate model.


#### setup information ############################################################################

## load packages
suppressPackageStartupMessages({
  require(raster)
  require(tidyverse)
  require(foreach)
  require(parallel)
  require(doSNOW)
})

## set up parallel backend
num.cores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK"))

## define helper functions
toNumber <- function(x) as.numeric(paste(x))
Sum <- function(x) sum(x, na.rm = TRUE)
Mean <- function(x) mean(x, na.rm = TRUE)

## define job number
job <- toNumber(commandArgs(trailingOnly=TRUE)[1])

## load data
load('samples.Rdata') #contains samples (columns = sim, cv, Qp, tp)
load('nonzero.Rdata') #contains nonzero, nonzero.buffer, & points

## define hyperparameter levels to consider
n.list <- c(1:6, 8, 10, 12, 15, 20)
p.list <- c(seq(0, 1, 0.25), 1.5, 2, 2.5, 3:5)
alpha.list <- seq(0.05, 0.95, 0.05)


###################################################################################################

## define IDW surrogate function
surrogate <- function(dist.matrix, i, n, p) {
  ## get the closest maps + associated weights
  distances <-
    data.frame(id = train.id, sim = train.sim,
               dist = dist.matrix[train.id, test.id[i]]) %>% arrange(dist)
  nearest <- distances[1:n, 'sim']
  weights <- 1/(distances[1:n, 'dist']^p)
  weights <- weights/sum(weights)
  
  ## find the prediction based on weighted average
  files <- paste0('../grid_final/results/max/gridded', nearest, '.max')
  sim <- foreach(ii = 1:n, .combine = '+') %do% (raster(files[ii])*weights[ii])
  return(sim)
}


###################################################################################################

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
      obs <- paste0('../grid_final/results/max/gridded', test.sim[i], '.max') %>% raster
      
      ## calculate predicted raster
      dist.matrix <- samples_scale %>% 
        transmute(Qp = Qp.norm*alpha, tp = tp.norm*(1-alpha)) %>% 
        dist(method = 'euclidean') %>% as.matrix
      sim <- surrogate(dist.matrix, i, n = n, p = p)

      ## return results as dataframe
      pts <- (sim-obs)[points]
      data.frame(
        sim = test.sim[i], 
        n = n, p = p, alpha = alpha,
        SRSS = sqrt(Sum(pts^2)),
        RMSE = sqrt(Mean(pts^2)),
        max.resid = pts[which.max(abs(pts))],
        max.loc = points[which.max(abs(pts))]
      )
    }
stopCluster(cl)
Sys.time() - start

## save results
write.csv(error, file = paste0('error', job, '.csv'))


###################################################################################################
