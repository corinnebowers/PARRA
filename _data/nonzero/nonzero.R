
###################################################################################################
## nonzero.R: creates nonzero.Rdata
## Corinne Bowers
## 6/28/2021
###################################################################################################

## packages
require(raster)
require(sf)
require(tidyverse)

## functions
toNumber <- function(x) as.numeric(paste(x))
raster.df <- function(x) as.data.frame(x, xy = TRUE) %>% setNames(c('x', 'y', 'value'))

## setup
setwd('D:/1-PARRA/_data/')

load('lisflood/dem.Rdata')
load('aoi/aoi.Rdata')
load('samples/samples.Rdata')


###################################################################################################

## find nonzero cells (cells that flooded in at least one simulation)
start <- Sys.time()
pb <- txtProgressBar(min = 0, max = 5000, style = 3)
cl <- parallel::makeCluster(round(detectCores()*2/3))
registerDoSNOW(cl)
nonzero <-
  foreach (
    i = 1:5001, .combine = '+', .packages = 'raster', .inorder = FALSE,
    .options.snow = list(progress = function(n) setTxtProgressBar(pb, n))) %dopar% {
      if (i %in% good) {
        file <- paste0('samples/results/gridded', i, '.max')
        raster(file) > 0
      } else {
        dem*0
      }
    }
stopCluster(cl)
Sys.time() - start

## get rid of Pacific Ocean cells
crs(nonzero) <- crs(dem)
nonzero <- nonzero %>% overlay(dem, fun = function(x, y) ifelse(y<=1, NA, x))

## buffer nonzero cells to create a conservative boundary
nonzero.buffer <- nonzero %>%
  raster.df %>%
  filter(!is.na(value) & value > 0) %>%
  mutate(value = 1) %>%
  rasterFromXYZ %>%
  rasterToPolygons %>%
  st_as_sf %>%
  st_union %>%
  st_set_crs(6417) %>%
  st_cast(to = 'POLYGON') %>%
  lapply(function(x) x[1]) %>%
  st_multipolygon %>%
  st_buffer(100) %>% #meters
  as('Spatial') %>%
  rasterize(dem) %>% 
  overlay(dem, fun = function(x, y) ifelse(y<=1, NA, x))

## extract ids for nonzero cells
points <- which(!is.na(nonzero.buffer[]))

## gut-check plot
ggplot() + 
  geom_raster(data = raster.df(nonzero.buffer) %>% filter(!is.na(value)), 
              aes(x=x, y=y, fill='buffer')) + 
  geom_raster(data = raster.df(nonzero) %>% filter(!is.na(value)) %>% filter(value>0), 
              aes(x=x, y=y, fill='nonzero')) + 
  scale_fill_discrete(na.value = NA) + 
  coord_sf(crs = crs(dem))


###################################################################################################

## save out for Sherlock
save(nonzero, nonzero.buffer, points, file = 'C:/Users/cbowers/Desktop/nonzero.Rdata')

