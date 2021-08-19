## this code generates .bdy and .bci files for LISFLOOD event analysis.

#### setup information ############################################################################
print('defining input information...')

## load packages
suppressPackageStartupMessages({
  require(lhs)
  require(dplyr)
})

## constants
mft <- 3.28084

## input parameters
n <- 1000  #number of LHS samples
m <- 4.5  #hydrograph shape parameter
simlength <- 30*24*3600  #simulation length (s)
spinup <- 30*24*3600  #simulation spin-up time (s)

##if the first simulation does not reach the outlet: 
#simlength <- 60*24*3600  #simulation length (s)
#spinup <- 30*24*3600  #simulation spin-up time (s)

baseflow <- 3  #baseflow (m3/s)
fileloc <- './bci_bdy/' #location to save files


#### generate samples #############################################################################
print('generating samples...')

load('edges.Rdata')

samples <- improvedLHS(n = n, k = 2) 
samples <- samples %>% 
  as.data.frame %>%
  setNames(paste0('x', 1:2)) %>%
  mutate(tp = (x1*2) %>% exp %>% 
           punif(min = 1, max = exp(2)) %>% 
           qunif(min = 0, max = 240),
         Qp = (x2*2) %>% exp %>% 
           punif(min = 1, max = exp(2)) %>% 
           qunif(min = 0, max = 4000)) %>% 
  select(tp, Qp)
write.table(samples, file = 'samples_grid.txt',
            row.names = FALSE, quote = FALSE, sep = '\t')

##load from file, if necessary
#samples <- read.csv('samples_grid.txt', sep = '\t', header = TRUE)


#### generate bci and bdy files ###################################################################
print('creating files...')

pb <- txtProgressBar(min = 0, max = n, style = 3)
for (i in 1:n) {
#for (i in unlist(unname(read.table('id2.txt', header = FALSE)))) {
  ## define LHS parameters
  tp <- samples[i, 'tp']*3600  #seconds
  Qp <- samples[i, 'Qp']  #m3/s

  ## calculate storm hydrograph
  t <- seq(0, simlength, 60)
  q <- ((t/tp)^m * exp(m*(1-(t/tp)))) * Qp
  q <- cbind(q, baseflow) %>% apply(1, max)

  ## add spinup time
  t <- c(seq(0, spinup-1, 3600), spinup+t)
  q <- c(rep(baseflow, length(seq(0, spinup-1, 3600))), q)

  ## convert to m2/s
  edgewidth <- diff(edge.in$x)/2
  q <- q / edgewidth
  
  ## write out files
  bdy <- matrix(c('LISFLOOD', NA, paste0('gridflow', i), NA, length(t), 'seconds'), 
                byrow = TRUE, ncol = 2) %>% rbind(cbind(q, t))
  write.table(bdy, file = paste0('bci_bdy/gridflow', i, '.bdy'), 
              row.names = FALSE, col.names = FALSE, quote = FALSE, sep = '\t', na = '')
  bci <- data.frame(matrix(
    c('N', round(min(edge.in$x)), round(max(edge.in$x)), 'QVAR', paste0('gridflow', i),
      'W', round(min(edge.out$y)), round(max(edge.out$y)), 'FREE', NA),
    nrow = 2, byrow = TRUE))
  write.table(bci, file = paste0('bci_bdy/gridflow', i, '.bci'), 
              row.names = FALSE, col.names = FALSE, quote = FALSE, sep = '\t', na = '')

  setTxtProgressBar(pb, i)
}
cat('\n')


###################################################################################################
print('done!')
