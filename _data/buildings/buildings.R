
###################################################################################################
## buildings.R: creates parcels.Rdata & buildings.Rdata
## Corinne Bowers
## 6/28/2021
###################################################################################################

## packages
library(tigris)
library(sf)
library(raster)
library(tidyverse)
require(censusapi); Sys.setenv(CENSUS_KEY = 'f2e090156b02ced027d4ed756f82c9a3a1aa38c9')

## functions

## setup
setwd('D:/1-PARRA/_data/')

## load area of interest (aoi)
load('aoi/aoi.Rdata')


###################################################################################################
#### generate parcels.Rdata ####

## load buildings & parcels (takes a while)
parcels <-
  st_read('https://opendata.arcgis.com/datasets/2202c1cd6708441f987ca5552f2d9659_0.geojson', 
          quiet = TRUE) %>%
  st_transform(6417) %>% st_intersection(aoi)
bldg_sonoma <- 
  st_read(paste0('https://socogis.sonomacounty.ca.gov/map/rest/services/BASEPublic/Buildings/',
                 'FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson'), quiet = TRUE) %>%
  st_transform(6417) %>% st_intersection(aoi)

## save results
save(parcels, bldg_sonoma, file = 'parcels.Rdata')


###################################################################################################
#### generate buildings.Rdata ####

## load buildings & parcels from file
# (contains parcels dataframe & bldg_sonoma dataframe)
load('parcels.Rdata')

## load nonzero cells
# (contains ____)
load('nonzero.Rdata')

## crop buildings & parcels to only nonzero cells
nonzero.buffer <- nonzero.buffer %>%
  rasterToPolygons %>% 
  st_as_sf %>% st_union %>% 
  st_transform(6417)
bldg_buffer <- bldg_sonoma[nonzero.buffer,] %>%
  mutate(bldg_sqft = st_area(.)*mft^2) %>% 
  st_centroid
buildings <- st_intersection(bldg_buffer, parcels)

## identify residential buildings
residential <- c(
  "ATTACHED UNIT",
  "CONDOMINIUM UNIT",
  "DETACHED UNIT IN A PUD",
  "DUET",
  "ENFORCEABLY RESTRICTED DWELLING",
  "MANUFACTURED HOME CONDOMINIUM LOT",
  "MANUFACTURED HOME ON URBAN LOT",
  "ONE DUPLEX (ONE STRUCTURE)",
  "RURAL RES SFD W/GRANNY UNIT",
  "RURAL RES W/MISC RES IMP",
  "RURAL RES/2 OR MORE RES",
  "RURAL RES/MANUFACTURED HOME",
  "RURAL RES/SECONDARY USE",
  "RURAL RES/SINGLE RES",
  "SFD SECONDARY USE",
  "SFD W/GRANNY UNIT",
  "SINGLE FAMILY DWELLING",
  "SINGLE LIVE/WORK UNIT",
  "TAXABLE MANUFACTURED HOME/CONDO LOT",
  "TAXABLE MANUFACTURED HOME/RENTED SITE",
  "TWO SFD ON SINGLE PARCEL",
  "WILDCAT SUBDIVISION LOT"
  )

## keep relevant columns & valuation codes
buildings <- buildings %>%
  filter(UseCType == 'Residential') %>%
  filter(UseCDesc %in% residential) %>%
  select("APN", "TxbltyDesc", "UseCDesc", "UseCType", "LndSzSF", "V601RollYr", "V601Land", 
         "V601Stru", "V601Fix", "V601FixRP", "V601Grow", "V601TotalL", "bldg_sqft") %>%
  mutate(bldg_sqft = as.numeric(bldg_sqft))

## filter out cases when a parcel has multiple structures
buildings <- buildings %>%
  cbind(st_coordinates(.)) %>%
  st_drop_geometry %>%
  group_by(APN) %>%
  summarize(across(everything(), ~.x[which.max(toNumber(bldg_sqft))])) %>%
  st_as_sf(coords = c('X', 'Y'), crs = 6417)

## attach geographic information
sonoma_tracts <- tracts(state = 'CA', county = 'Sonoma')
sonoma_cbgs <- block_groups(state = 'CA', county = 'Sonoma')
sonoma_blocks <- blocks(state = 'CA', county = 'Sonoma')
buildings <- buildings %>%
  st_intersection(sonoma_tracts %>% st_transform(6417) %>% transmute(tract = GEOID)) %>%
  st_intersection(sonoma_cbgs %>% st_transform(6417) %>% transmute(blockgroup = GEOID)) %>%
  st_intersection(sonoma_blocks %>% st_transform(6417) %>% transmute(block = GEOID10))


###################################################################################################

## download RESA data
# https://sonomacounty.maps.arcgis.com/apps/webappviewer/index.html?id=1cab5991f10643b1bc7c16e7769887c2
# https://sonomacounty.maps.arcgis.com/home/item.html?id=9e5d8762b5554765912f591b7540fed4
# instructions: open URL in ArcGIS Pro, then copy-paste Attributes table into Excel
read.csv('buildings_resa.csv')
resa[resa == '<Null>'] <- NA

## attach RESA tag information
buildings <- buildings %>% 
  mutate(bldg = 1:nrow(.)) %>% 
  left_join(resa %>% transmute(id = OBJECTID, APN, RESA_Status_GIS, Within46ftInundation), 
            by = 'APN') %>% 
  mutate(status = case_when(RESA_Status_GIS != 'Orange' ~ RESA_Status_GIS) %>% 
           factor(levels = c('Red', 'Yellow', 'Green', 'N/A')))


###################################################################################################

## find ACS median value of owner-occupied units by census tract
acs <- 
  getCensus(name = "acs/acs5", vintage = 2018, region = "tract:*", 
            regionin = "state:06+county:097", vars = "group(B25077)") %>%
  mutate(GEOID = paste0(state, county, tract) %>% toNumber) %>% 
  select(-c(GEO_ID, state, county, tract, NAME)) %>% 
  transmute(GEOID, acs.median = B25077_001E, MOE = B25077_001M) %>% 
  # mutate(sd = MOE/qnorm(0.95)) %>% 
  mutate(acs.05 = acs.median-MOE, acs.95 = acs.median+MOE) %>% 
  filter(acs.median > 0)

## plot ACS vs. Sonoma tax assessor roll values
buildings %>%
  st_drop_geometry %>% 
  group_by(tract) %>% 
  summarize(parcel.median = median(V601TotalL), 
            parcel.05 = quantile(V601TotalL, 0.05), 
            parcel.95 = quantile(V601TotalL, 0.95)) %>% 
  left_join(acs %>% transmute(tract = GEOID, acs.median, acs.05, acs.95), by = 'tract') %>% 
  filter(tract != 6097153808) %>% 
  ggplot() + 
  geom_segment(aes(x = acs.median, xend = acs.median, 
                   y = parcel.05, yend = parcel.95), color = 'grey70') + 
  geom_segment(aes(x = acs.05, xend = acs.95, 
                   y = parcel.median, yend = parcel.median), color = 'grey70') + 
  geom_point(aes(x = acs.median, y = parcel.median)) + 
  scale_x_origin('ACS Owner-Occupied Units (B25077)', 
                 labels = comma_format(scale = 1e-6, prefix = '$', suffix = 'M', accuracy = 0.1)) + 
  scale_y_origin('Sonoma Tax Assessor Roll', 
                 labels = comma_format(scale = 1e-6, prefix = '$', suffix = 'M', accuracy = 0.1)) + 
  ggtitle('Median SFH Valuation by Tract', subtitle = 'Sonoma County, 2019') + 
  geom_parity() + coord_fixed()

## plot ACS vs. Sonoma tax assessor roll uncertainty
## (differs by about a factor of 10)
buildings %>% 
  group_by(tract) %>% 
  summarize(parcel.median = median(V601TotalL), 
            parcel.05 = quantile(V601TotalL, 0.05), 
            parcel.95 = quantile(V601TotalL, 0.95)) %>% 
  left_join(acs %>% transmute(tract = GEOID, acs.median, acs.05, acs.95), by = 'tract') %>% 
  filter(tract != 6097153808) %>% 
  ggplot() + 
  geom_point(aes(x = acs.95-acs.05, y = parcel.95-parcel.05)) + 
  scale_x_origin('ACS Owner-Occupied Units (B25077)', 
                 labels = comma_format(scale = 1e-6, prefix = '$', suffix = 'M', accuracy = 0.1)) + 
  scale_y_origin('Sonoma Tax Assessor Roll', 
                 labels = comma_format(scale = 1e-6, prefix = '$', suffix = 'M', accuracy = 0.1)) + 
  ggtitle('Valuation Margin of Error') + 
  geom_parity() + coord_fixed()

## attach valuation + uncertainty information
buildings <- buildings %>% 
  left_join(acs %>% transmute(tract = GEOID, acs_value = value, acs_sd = sd), by = 'tract')


###################################################################################################

## save results
save(buildings, file = 'buildings.Rdata')



