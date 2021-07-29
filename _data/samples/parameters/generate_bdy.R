
#### setup information ############################################################################

## this code generates .bdy and .bci files for LISFLOOD event analysis.

## load packages
require(lhs)
require(dplyr)

## constants
mft <- 3.28084

## input parameters
n <- 5000  #number of LHS samples
m <- 4  #hydrograph shape parameter
simlength <- 30*24*3600  #simulation length (s)
baseflow <- 3  #baseflow (m3/s)
drainarea <- 655 * 5280^2 / mft^2  #drainage area (m2)
fileloc <- './bci_bdy/' #location to save files
spinup <- 15*24*3600  #simulation spin-up time (s)


#### generate samples #############################################################################
print('generating samples...')

load('edges_new.Rdata')
samples <- improvedLHS(n = n, k = 2) 
samples <- samples %>% 
  as.data.frame %>%
  setNames(paste0('x', 1:2)) %>%
  mutate(tp = (x1*2) %>% exp %>% 
           punif(min = 1, max = exp(2)) %>% 
           qunif(min = 6, max = 200),
         Qp = (x2*2) %>% exp %>% 
           punif(min = 1, max = exp(2)) %>% 
           qunif(min = baseflow, max = 8000)) %>% 
  select(tp, Qp)
write.table(samples, file = 'samples_grid.txt',
            row.names = FALSE, quote = FALSE, sep = '\t')

#samples = read.csv('samples_grid.txt', sep = '\t', headers = TRUE)


#### generate bci and bdy files ###################################################################
print('creating files...')

for (i in 1:n) {
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
  q <- q / mean(edge.in$layer)
  
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
}
