## this code generates .bdy and .bci files for LISFLOOD event analysis.

#### setup information ############################################################################
print('defining input information...')

## set seed for reproducibility
set.seed(2021)

## setup information
setwd('/home/groups/bakerjw/cbowers/PARRA/')
#setwd('D:/1-PARRA/')
source('_data/setup.R')

## load required packages
require(lhs)

## load necessary information
load('_data/lisflood/edges.Rdata')

## move to working folder
setwd('./_scripts/5_INUN/fit_inundation/5d_populate_grid/')

## parallel backend
num_cores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK"))
#num_cores <- 5


#### define user input parameters #################################################################

n <- 50  			#number of LHS samples
m <- 4.5  			#hydrograph shape parameter
baseflow <- 3  			#baseflow (m3/s)

## for the first round of simulations:
hydrolength <- 30*24*3600  	#simulation length (s)
spinup <- 30*24*3600  		#simulation spin-up time (s)
## if the first simulation does not reach the outlet: 
#hydrolength <- 50*24*3600  	#simulation length (s)
#spinup <- 100*24*3600  	#simulation spin-up time (s)


#### generate samples #############################################################################
print('generating samples...')

## generate LHS samples
samples <- improvedLHS(n = n, k = 2) %>% 
  as.data.frame %>%
  setNames(paste0('x', 1:2)) 

## convert uniform numbers to fill sample space
samples <- samples %>%
  mutate(tp = (x1*2) %>% exp %>% 
           punif(min = 1, max = exp(2)) %>% 
           qunif(min = 0, max = 240),
         Qp = (x2*2) %>% exp %>% 
           punif(min = 1, max = exp(2)) %>% 
           qunif(min = 0, max = 4000)) %>% 
  select(tp, Qp)

## save to file
write.table(samples, file = 'samples_grid.txt',
            row.names = FALSE, quote = FALSE, sep = '\t')


#### generate bci and bdy files ###################################################################
print('creating files...')

## load samples from file, if necessary
#samples <- read.csv('samples_grid.txt', sep = '\t', header = TRUE)

## convert Qp & tp information into LISFLOOD files
cl <- parallel::makeCluster(num_cores)
pb <- txtProgressBar(min = 0, max = n, style = 3)
registerDoSNOW(cl)
null <- foreach (i = 1:n,
  .options.snow = list(progress = function(n) setTxtProgressBar(pb, n)),
  .packages = c('dplyr')) %dorng% {

    ## define LHS parameters
    tp <- samples[i, 'tp']*3600  #seconds
    Qp <- samples[i, 'Qp']  #m3/s

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
      c('LISFLOOD', NA, paste0('grid', i), NA, length(t), 'seconds'), 
      byrow = TRUE, ncol = 2) %>% rbind(cbind(q, t))
    write.table(bdy, 
      file = paste0('files/bdy/grid', i, '.bdy'), 
      row.names = FALSE, col.names = FALSE, quote = FALSE, sep = '\t', na = '')

    ## write out .bci file
    bci <- data.frame(matrix(
      c('N', round(min(edge.in$x)), round(max(edge.in$x)), 'QVAR', paste0('grid', i),
        'W', round(min(edge.out$y)), round(max(edge.out$y)), 'FREE', NA),
      nrow = 2, byrow = TRUE))
    write.table(bci, 
      file = paste0('files/bci/grid', i, '.bci'), 
      row.names = FALSE, col.names = FALSE, quote = FALSE, sep = '\t', na = '')
  }
stopCluster(cl)
cat('\n')

# for (i in unlist(unname(read.table('id.txt', header = FALSE)))) {


#### output simulation length #####################################################################
print('saving simulation length as a known constant...')
write.table(spinup + hydrolength,
  file = 'files/simlength.txt', 
  row.names = FALSE, col.names = FALSE, quote = FALSE, sep = '\t', na = '')


###################################################################################################
print('done!')
