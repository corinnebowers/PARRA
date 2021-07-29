
###################################################################################################
## foundations.R: creates foundations.Rdata
## Corinne Bowers
## 6/29/2021
###################################################################################################

## packages
library(tigris)
library(sf)
library(tidyverse)

## functions
toNumber <- function(x) as.numeric(paste(x))

## setup
setwd('D:/1-PARRA/_data/')


###################################################################################################

## load NSI data
nsi1 <- read.csv('foundations_nsi1.csv') %>% 
  st_as_sf(coords = c('X', 'Y'), crs = 4269)

## summarize NSI foundation info by census tract
sonoma <- tracts(state = 'CA', county = 'Sonoma')
nsi1.found <- nsi1 %>% 
  st_intersection(sonoma) %>% 
  st_drop_geometry 
nsi1.found <- nsi1.found %>% 
  mutate(GEOID = toNumber(GEOID)) %>% 
  mutate(counter = 1) %>% 
  group_by(GEOID, Found_Ht, Found_Type) %>% 
  summarize(n = sum(counter), .groups = 'drop') %>% 
  mutate(foundation = paste(Found_Type, Found_Ht, sep = '_')) %>% 
  pivot_wider(id_cols = 'GEOID', names_from = 'foundation', values_from = 'n') %>% 
  dplyr::select(-`SolidWall_7`)
nsi1.found[is.na(nsi1.found)] <- 0
nsi1.found[,-1] <- nsi1.found[,-1] / apply(nsi1.found[,-1], 1, sum)


###################################################################################################

## save results
save(nsi1.found, file = 'foundations.Rdata')


