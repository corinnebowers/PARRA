
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


#### define user input parameters #################################################################

simlength <- 40*24*3600  	#LISFLOOD simulation length (s)
spinup <- 30*24*3600  		#LISFLOOD simulation spin-up time (s)
baseflow <- 3  			#baseflow @ USGS 11463500 (m3/s)
Qp <- 112000 / (mft^3)  	#peak streamflow @ USGS 11463500 (m3/s)


#### generate .bci & .bdy files ###################################################################
print('generating .bci & .bdy files...')

## convert tp, m, & edge information into LISFLOOD files
tp <- 44 * 3600  #seconds
m <- 4

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
bdy <- matrix(c('LISFLOOD', NA, 'bestfit', NA, length(t), 'seconds'), 
              byrow = TRUE, ncol = 2) %>% rbind(cbind(q, t))
write.table(bdy, file = 'bestfit.bdy', 
            row.names = FALSE, col.names = FALSE, quote = FALSE, sep = '\t', na = '')
bci <- data.frame(matrix(
  c('N', round(min(edge.in$x)), round(max(edge.in$x)), 'QVAR', 'bestfit',
    'W', round(min(edge.out$y)), round(max(edge.out$y)), 'FREE', NA),
  nrow = 2, byrow = TRUE))
write.table(bci, file = 'bestfit.bci', 
            row.names = FALSE, col.names = FALSE, quote = FALSE, sep = '\t', na = '')


#### generate .n.asc files ########################################################################
print('generating .n.asc files...')

## get LULC codes
lulc <- rasterFromXYZ(lulc.df, crs = crs(lulc))

## grab the Manning's n values of interest
manning_values <- 
  cbind(code = toNumber(manning$code), 
        value = toNumber(manning$default))

## change LULC codes to roughness values
lulc.dem <- lulc %>% 
  reclassify(manning_values) %>% 
  projectRaster(blank)

## save out
writeRaster(lulc.dem, format = 'ascii', overwrite = TRUE, 
            filename = 'russian.n.asc')


###################################################################################################
print('done!')
