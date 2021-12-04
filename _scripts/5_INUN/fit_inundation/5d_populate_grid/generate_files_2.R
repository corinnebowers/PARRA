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

## load samples from file
samples <- read.csv('samples_grid.txt', sep = '\t', header = TRUE)

## parallel backend
num_cores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK"))
#num_cores <- 5


#### define user input parameters #################################################################

n <- 1000 			#number of LHS samples
m <- 4  			#hydrograph shape parameter
baseflow <- 3  			#baseflow (m^3/s)
#NOTE: all of these should be the same as the values in generate_files.R

## if the first simulation does not reach the outlet: 
hydrolength <- 50*24*3600  	#simulation length (s)
spinup <- 100*24*3600  		#simulation spin-up time (s)


#### identify incomplete simulations ##############################################################
print('identifying incomplete simulations...')

## remove simulations that errored out in Sherlock
files <- list.files('results/max')
zip <- grep('grid', files, invert = TRUE)
files <- files[-zip]
sims <- files %>% str_remove('grid') %>% str_remove('.max') %>% toNumber
bad1 <- (1:n)[!(1:n %in% sims)]

## remove simulations that do not reach the ocean
pt <- data.frame(lat = 38.45166, lon = -123.12934) %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>% 
  st_transform(6417) %>% 
  st_coordinates
filenames <- list.files('results/max', full.names = TRUE)[-zip]
pb <- txtProgressBar(min = 0, max = length(files), style = 3)
cl <- parallel::makeCluster(round(detectCores()*2/3))
registerDoSNOW(cl)
badfile <-
  foreach (
    file = filenames, .combine = 'c',
    .options.snow = list(progress = function(n) setTxtProgressBar(pb, n)),
    .packages = c('raster', 'terra', 'sf', 'dplyr'), .inorder = FALSE) %dopar% {
      if (rast(file) %>% terra::extract(pt) == 0) which(file == filenames)
    }
stopCluster(cl)
cat('\n')
bad2 <- files[badfile] %>% str_remove('grid') %>% str_remove('.max') %>% toNumber

## save to file
id <- sort(unique(c(bad1, bad2)))
write.table(id, 
  file = 'id.txt',
  row.names = FALSE, col.names = FALSE)


#### generate bci and bdy files ###################################################################
print('creating files...')

## load indexes from file, if necessary
#id <- unlist(unname(read.table('id.txt', header = FALSE)))

## convert Qp & tp information into LISFLOOD files
if (length(id) > 0) {
  cl <- parallel::makeCluster(num_cores)
  pb <- txtProgressBar(min = 0, max = n, style = 3)
  registerDoSNOW(cl)
  null <- foreach (i = id,
    .options.snow = list(progress = function(n) setTxtProgressBar(pb, n)),
    .packages = c('dplyr')) %dopar% {
  
      ## define LHS parameters
      tp <- samples[i, 'tp']*3600  #seconds
      Qp <- samples[i, 'Qp']  	 #m^3/s
  
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
}


#### output simulation length #####################################################################
print('saving simulation length as a known constant...')
write.table(spinup + hydrolength,
  file = 'files/simlength_2.txt', 
  row.names = FALSE, col.names = FALSE, quote = FALSE, sep = '\t', na = '')


###################################################################################################
print('done!')
