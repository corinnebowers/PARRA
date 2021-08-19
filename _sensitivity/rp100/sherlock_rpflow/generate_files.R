
###################################################################################################

## generate_files.R
## Corinne Bowers
## 7/22/2021

## This code generates samples and files (.bci, .bdy, & .n.asc) for LISFLOOD event analysis. 


#### setup information ############################################################################
print('loading necessary packages and files...')

## load packages
suppressPackageStartupMessages({
  require(sf)
  require(raster)
  require(dplyr)
  require(foreach)
  require(parallel)
  require(doSNOW)
  require(lhs) 
  require(tidyverse)
})

## define constants
mft <- 3.28084

## define helper functions
toNumber <- function(x) as.numeric(paste(x))

## load necessary information
load('aoi.Rdata')
load('edges.Rdata')
load('lulc.Rdata')
manning <- read.table('manning_values.txt', header = TRUE) %>% as.data.frame

## parallel backend
num_cores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK"))


#### define user input parameters #################################################################

n <- 500  			#number of LHS samples
simlength <- 20*24*3600  	#LISFLOOD simulation length (s)
spinup <- 30*24*3600  		#LISFLOOD simulation spin-up time (s)
baseflow <- 3  			#baseflow @ USGS 11463500 (m3/s)
Qp <- 112000 / (mft^3)  	#peak streamflow @ USGS 11463500 (m3/s)


#### generate samples #############################################################################
print('generating Latin hypercube samples...')

## generate LHS samples
samples.lhs <- improvedLHS(n = n, k = 20) %>% 
  as.data.frame %>% setNames(paste0('x', 1:20)) 

## convert uniform numbers of distributions of interest
samples <- map_dfc(.x = 1:15,
  .f = ~qunif(samples.lhs[,.x], min = unlist(manning[.x, 'n.min']), 
              max = unlist(manning[.x, 'n.max']))) %>% 
  setNames(unlist(manning[,'code'])) %>% 
  mutate(SGCn = qunif(samples.lhs$x16, min = 0.015, max = 0.075),
         tp = qunif(samples.lhs$x17, min = 1, max = 80),
         SGCp = qunif(samples.lhs$x18, min = 0.69, max = 0.82),
         SGCr = qunif(samples.lhs$x19, min = 0.05, max = 0.5),
         m = qunif(samples.lhs$x20, min = 0, max = 10))

## save to file
write.table(samples, file = 'samples_rp100.txt',
            row.names = FALSE, quote = FALSE, sep = '\t')

## load from file, if necessary
#samples = read.csv('samples_rp100.txt', sep = '\t', header = TRUE)


#### generate .bci & .bdy files ###################################################################
print('generating .bci & .bdy files...')

## convert tp, m, & edge information into LISFLOOD files
for (i in 1:n) {
  ## define LHS parameters
  tp <- unlist(samples[i,'tp']) * 3600  #seconds
  m <- unlist(samples[i,'m'])

  ## calculate storm hydrograph
  t <- seq(0, simlength, 60)
  q <- ((t/tp)^m * exp(m*(1-(t/tp)))) * Qp
  q <- cbind(q, baseflow) %>% apply(1, max)

  ## add spinup time
  t <- c(seq(0, spinup-1, 3600), spinup+t)
  q <- c(rep(baseflow, length(seq(0, spinup-1, 3600))), q)
  
  ## convert to m2/s
  edgewidth <- diff(edge.in$x)/2  #m
  q <- q / edgewidth
  
  ## write out files
  bdy <- matrix(c('LISFLOOD', NA, paste0('rpflow', i), NA, length(t), 'seconds'), 
                byrow = TRUE, ncol = 2) %>% rbind(cbind(q, t))
  write.table(bdy, file = paste0('bci_bdy/rpflow', i, '.bdy'), 
              row.names = FALSE, col.names = FALSE, quote = FALSE, sep = '\t', na = '')
  bci <- data.frame(matrix(
    c('N', round(min(edge.in$x)), round(max(edge.in$x)), 'QVAR', paste0('rpflow', i),
      'W', round(min(edge.out$y)), round(max(edge.out$y)), 'FREE', NA),
    nrow = 2, byrow = TRUE))
  write.table(bci, file = paste0('bci_bdy/rpflow', i, '.bci'), 
              row.names = FALSE, col.names = FALSE, quote = FALSE, sep = '\t', na = '')
}


#### generate .n.asc files ########################################################################
print('generating .n.asc files...')

## get LULC codes
lulc <- rasterFromXYZ(lulc.df, crs = crs(lulc))

## convert LULC to Manning's n
cl <- parallel::makeCluster(num_cores)
pb <- txtProgressBar(min = 0, max = n, style = 3)
registerDoSNOW(cl)
foreach (i = 1:n, 
  .packages = c('sf', 'raster', 'dplyr'), 
  .options.snow = list(progress = function(n) setTxtProgressBar(pb, n)),
  .export = 'toNumber') %dopar% {
    ## grab the Manning's n values of interest
    manning_values <- 
      cbind(code = names(samples)[1:15] %>% gsub('X', '', .) %>% toNumber, 
            value = unlist(samples[i, 1:15]))

    ## change LULC codes to roughness values
    lulc.dem <- lulc %>% 
      reclassify(manning_values) %>% 
      projectRaster(blank)

    ## save out
    writeRaster(lulc.dem, format = 'ascii', overwrite = TRUE, 
                filename = paste0('manning/russian.n', i, '.asc'))
}
stopCluster(cl)


###################################################################################################
print('done!')
