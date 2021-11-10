
###################################################################################################

## generate_files.R
## Corinne Bowers
## 7/22/2021

## This code generates samples and files (.bci, .bdy, & .n.asc) for LISFLOOD event analysis. 


#### setup information ############################################################################
print('loading necessary packages and files...')

## set seed for reproducibility
set.seed(2021)

## setup information
setwd('/home/groups/bakerjw/cbowers/PARRA/')
#setwd('D:/1-PARRA/')
source('_data/setup.R')

## load required packages
require(lhs)

## load necessary information
load('_data/aoi/aoi.Rdata')
load('_data/lisflood/edges.Rdata')
load('_data/lulc/lulc.Rdata')
manning <- read.table('_data/lulc/manning_values.txt', header = TRUE) %>% as.data.frame

## move to working folder
setwd('./_scripts/5_INUN/fit_inundation/5b_run_bestfit/')


#### define best-fit parameters from rp100.Rmd ####################################################

hydrolength <- 20*24*3600	#length of LISFLOOD hydrograph (s)
spinup <- 12*3600	#30*24*3600  		#LISFLOOD simulation spin-up time (s)
baseflow <- 3  			#baseflow @ USGS 11463500 (m3/s)
Qp <- 112000 / (mft^3)  	#peak streamflow @ USGS 11463500 (m3/s)

tp <- 44 * 3600			#time to peak streamflow (sec)
m <- 4.5			#hydrograph shape parameter

manning[toNumber(manning$code) == 82, 'default'] <- 0.034
manning[toNumber(manning$code) == 71, 'default'] <- 0.32


#### generate .bci & .bdy files ###################################################################
print('generating .bci & .bdy files...')

## calculate storm hydrograph
t <- seq(0, hydrolength, 60)
q <- ((t/tp)^m * exp(m*(1-(t/tp)))) * Qp
q <- cbind(q, baseflow) %>% apply(1, max)

## add spinup time
t <- c(seq(0, spinup-1, 3600), spinup+t)
q <- c(rep(baseflow, length(seq(0, spinup-1, 3600))), q)
  
## convert to m2/s
edgewidth <- diff(edge.in$x)/2  #m
q <- q / edgewidth
  
## write out .bdy file
bdy <- matrix(
  c('LISFLOOD', NA, 'bestfit', NA, length(t), 'seconds'), 
  byrow = TRUE, ncol = 2) %>% rbind(cbind(q, t))
write.table(bdy, 
  file = 'files/bestfit.bdy', 
  row.names = FALSE, col.names = FALSE, quote = FALSE, sep = '\t', na = '')
    
## write out .bci file
bci <- data.frame(matrix(
  c('N', round(min(edge.in$x)), round(max(edge.in$x)), 'QVAR', 'bestfit',
    'W', round(min(edge.out$y)), round(max(edge.out$y)), 'FREE', NA),
  nrow = 2, byrow = TRUE))
write.table(bci, 
  file = 'files/bestfit.bci', 
  row.names = FALSE, col.names = FALSE, quote = FALSE, sep = '\t', na = '')


#### generate .n.asc file #########################################################################
print('generating .n.asc file...')

## get LULC codes
lulc <- rasterFromXYZ(lulc.df, crs = crs(lulc))

## grab the Manning's n values of interest
manning_values <- cbind(code = toNumber(manning$code), value = toNumber(manning$default))

## change LULC codes to roughness values
lulc.dem <- lulc %>% 
  reclassify(manning_values) %>% 
  projectRaster(blank)

## save out
writeRaster(lulc.dem, 
  format = 'ascii', overwrite = TRUE, 
  filename = 'files/russian.n.asc')


#### output simulation length #####################################################################
print('saving simulation length as a known constant...')
write.table(spinup + hydrolength,
  file = 'files/simlength.txt', 
  row.names = FALSE, col.names = FALSE, quote = FALSE, sep = '\t', na = '')


###################################################################################################
print('done!')
