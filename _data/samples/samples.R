
###################################################################################################
## samples.R: creates samples.Rdata
## Corinne Bowers
## 7/1/2021
###################################################################################################

## packages
require(sf)
require(raster)
require(terra)
require(tidyverse)

## functions

## setup


###################################################################################################

## load samples
samples <-
  read.table(paste0('C:/Users/cbowers/Desktop/LISFLOOD/sonoma_sherlock/',
                    '21-05-31 gridded/samples_grid.txt'), header = TRUE) %>%
  mutate(sim = 1:nrow(.)) %>% 
  rbind(c(tp = 1e-6, Qp = 1e-6, sim = 5001))  #add a lower bound point


###################################################################################################

## remove simulations that errored out in Sherlock
files <- list.files(paste0('samples/results'))
sims <- files %>% gsub('gridded', '', .) %>% gsub('.max', '', .) %>% toNumber
bad1 <- (1:5000)[!(1:5000 %in% sims)]

## remove simulations that do not reach the ocean
pt <- data.frame(lat = 38.45064279, lon = -123.12917330) %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>% 
  st_transform(6417) %>% 
  st_coordinates
filenames <- list.files(paste0('samples/results'), full.names = TRUE)

pb <- txtProgressBar(min = 0, max = length(files), style = 3)
cl <- parallel::makeCluster(round(detectCores()*2/3))
registerDoSNOW(cl)
badfile <-
  foreach (
    file = filenames, .combine = 'c',
    .packages = c('raster', 'terra', 'sf', 'dplyr'), .inorder = FALSE,
    .options.snow = list(progress = function(n) setTxtProgressBar(pb, n))) %dopar% {
      if (rast(file) %>% terra::extract(pt) == 0) {
        which(file == filenames)
      }
    }
stopCluster(cl)
bad2 <- files[badfile] %>% str_remove('gridded') %>% str_remove('.max') %>% toNumber

## keep successful simulations
good <- c((1:5000)[-c(bad1, bad2)], 5001)

## plot failed simulations within the sample space
ggplot(samples %>% arrange(!(sim %in% good))) + 
  geom_point(aes(x = Qp, y = tp, color = !sim %in% good)) + 
  scale_color_manual('Failed \nSimulations', values = c('grey80', 'red')) + 
  scale_x_origin('Peak Flow, Qp (m3/s)', labels = comma) + 
  scale_y_origin('Time to Peak Flow, tp (hrs)', labels = comma)
ggplot(samples %>% filter(sim %in% good)) +
  geom_point(aes(x = Qp, y = tp)) + 
  geom_point(data = catalog, aes(x = Qp/mft^3, y = tp), color = ggcolor(3)[2])


###################################################################################################

## load catalog
load('catalog/catalog.Rdata')

## plot where real events fall within the sample space
g <- ggplot(samples %>% arrange(!(sim %in% good))) + 
  geom_point(aes(x = Qp, y = tp, color = !sim %in% good)) + 
  scale_color_manual('Failed \nSimulations', values = c('grey80', 'red')) + 
  scale_x_origin('Peak Flow, Qp (m3/s)', labels = comma) + 
  scale_y_origin('Time to Peak Flow, tp (hrs)', labels = comma)
g + geom_point(data = catalog, aes(x = Qp/mft^3, y = tp))


###################################################################################################

## add a CV column
cv <- sample(1:10, size = length(good), replace = TRUE)
samples <- samples %>% mutate(cv = replace(NA, sim %in% good, cv))

## save out 
save(samples, file = 'samples/samples.Rdata')


