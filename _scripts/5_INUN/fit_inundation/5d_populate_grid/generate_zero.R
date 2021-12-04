## this code generates .bdy and .bci files for LISFLOOD event analysis.

#### setup information ############################################################################
print('defining input information...')

## setup information
setwd('/home/groups/bakerjw/cbowers/PARRA/')
#setwd('D:/1-PARRA/')
source('_data/setup.R')

## load necessary information
load('_data/lisflood/edges.Rdata')

## move to working folder
setwd('./_scripts/5_INUN/fit_inundation/5d_populate_grid/')


#### define user input parameters #################################################################

n <- 1000			#number of LHS samples
m <- 4  			#hydrograph shape parameter
baseflow <- 3  			#baseflow (m^3/s)
#NOTE: all of these should be the same as the values in generate_files.R

hydrolength <- 50*24*3600  	#simulation length (s)
spinup <- 100*24*3600  		#simulation spin-up time (s)


#### generate bci and bdy files ###################################################################
print('creating files...')

## generate .bci file with fixed inflow condition 
i <- n + 1
edgewidth <- diff(edge.in$x)/2
qfix <- baseflow / edgewidth

## write out .bci file
bci <- data.frame(matrix(
  c('N', round(min(edge.in$x)), round(max(edge.in$x)), 'QFIX', qfix,
    'W', round(min(edge.out$y)), round(max(edge.out$y)), 'FREE', NA),
  nrow = 2, byrow = TRUE))
write.table(bci, 
  file = paste0('files/bci/grid', i, '.bci'), 
  row.names = FALSE, col.names = FALSE, quote = FALSE, sep = '\t', na = '')


#### output simulation length #####################################################################
print('saving simulation length as a known constant...')
write.table(spinup + hydrolength,
  file = 'files/simlength_zero.txt',
  row.names = FALSE, col.names = FALSE, quote = FALSE, sep = '\t', na = '')


###################################################################################################
print('done!')
