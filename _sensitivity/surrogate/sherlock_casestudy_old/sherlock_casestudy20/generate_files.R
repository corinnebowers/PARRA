
###################################################################################################

## generate_files.R
## Corinne Bowers
## 8/12/2021

## This code generates files for LISFLOOD analysis of the 2019 case study storm. 


#### setup information ############################################################################
print('loading necessary packages and files...')

## load packages
suppressPackageStartupMessages({
  require(sf)
  require(raster)
  require(dataRetrieval)
  require(lubridate)
  require(exactextractr)
  require(tidyverse)
})

## define constants
mft <- 3.28084

## define helper functions
toNumber <- function(x) as.numeric(paste(x))

## load data 
load('catalog.Rdata')
load('edges.Rdata')
load('aoi.Rdata')

## identify case study storm 
casestudy <- catalog %>% filter(start_day == ymd('2019-02-25'))


#### generate .bci & .bdy files ###################################################################
print('generating .bci & .bdy files...')

## load USGS data for gage 11463500
param <- c('00060', '00065'); names(param) <- c('discharge_cfs', 'gageht_ft')
statcode <- c('00001', '00002', '00003', '00008'); names(statcode) <- c('max', 'min', 'mean', 'median')
sites <- readNWISsite(11463500)
flow <- readNWISdata(
  sites = 11463500, parameterCd = param, 
  startDate = ymd(casestudy$start_day) - days(30), 
  endDate = ymd(casestudy$end_day) + days(7), 
  service = 'iv', tz = 'America/Los_Angeles') %>% 
  renameNWISColumns

## transform to correct units
edgewidth <- diff(edge.in$x)/2  #m
flow <- flow %>% 
  transmute(t = toNumber(dateTime - dateTime[1]),  #seconds
            q = Flow_Inst / mft^3 / edgewidth)  #m2/s

## write out files
bdy <- matrix(c('LISFLOOD', NA, 'casestudy', NA, length(flow$t), 'seconds'), 
              byrow = TRUE, ncol = 2) %>% rbind(cbind(flow$q, flow$t))
write.table(bdy, file = 'casestudy.bdy', 
            row.names = FALSE, col.names = FALSE, quote = FALSE, sep = '\t', na = '')
bci <- data.frame(matrix(
  c('N', round(min(edge.in$x)), round(max(edge.in$x)), 'QVAR', 'casestudy',
    'W', round(min(edge.out$y)), round(max(edge.out$y)), 'FREE', NA),
  nrow = 2, byrow = TRUE))
write.table(bci, file = 'casestudy.bci', 
            row.names = FALSE, col.names = FALSE, quote = FALSE, sep = '\t', na = '')


#### generate .stage & .gauge files ###############################################################
print('generating .stage & .gauge files...')

## identify relevant USGS gages
gages <- whatNWISsites(stateCd = '06', countyCd = '097') %>% 
  filter(grepl('RUSSIAN', station_nm)) %>% 
  filter(str_length(site_no) == 8)
temp <- readNWISdata(
  sites = gages$site_no, parameterCd = param, 
  startDate = casestudy$start_day, 
  endDate = ymd(casestudy$end_day) + days(1), 
  service = 'iv', tz = 'America/Los_Angeles') %>% 
  renameNWISColumns %>% 
  filter(!is.na(Flow_Inst) | !is.na(GH_Inst))
gages <- gages %>% 
  filter(site_no %in% unique(temp$site_no)) %>% 
  filter(site_no != 11463500) %>% 
  arrange(site_no)

## load width raster
width <- raster('russian.width.asc', crs = projection(aoi))

## generate .gauge and .stage files
gages.write <- gages %>%
  st_as_sf(coords = c('dec_long_va', 'dec_lat_va'), crs = 4269) %>%
  st_transform(6417) %>%
  st_buffer(40) %>%
  exact_extract(width, ., progress = FALSE, include_xy = TRUE) %>%
  lapply(function(x) x[complete.cases(x),]) %>%
  lapply(function(x) x[which.max(x$coverage_fraction),]) %>%
  do.call(rbind, .) %>%
  mutate(direction = c('E', 'W', 'W', 'W', 'W')) %>%
  select(x, y, direction, value) %>%
  as.matrix %>%
  rbind(c(nrow(.), NA, NA, NA), .)
write.table(gages.write,
  file = 'russian.gauge', na = '',
  sep = '\t', row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(gages.write[,1:2],
  file = 'russian.stage', na = '',
  sep = '\t', row.names = FALSE, col.names = FALSE, quote = FALSE)


###################################################################################################
print('done!')
