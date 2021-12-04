
## This script downloads inundation scenarios provided by Sonoma County: 
# https://sonomacounty.maps.arcgis.com/home/item.html?id=9d8d63558c6b4124b000e6476a0a020d


#### download & save all inundation scenarios #####################################################

# setwd('D:/1-PARRA/')
# source('_data/setup.R')

# ## run the loop once & use this to narrow down the bounding box request
# prmd.extent <- extent(flood) %>%
#   as('SpatialPolygons') %>%
#   as('sf') %>%
#   st_set_crs(crs_ca)
# ext <- aoi %>%
#   st_transform(crs_ca) %>%
#   st_intersection(prmd.extent)
# st_bbox(ext)

# for (ht in 32:52) {
#   ## download Sonoma GIS data
#   url <- paste0(
#     'https://data.sonomaopenspace.org/arcgis/rest/services/Projects/',
#     'Guerneville_Gauge_at_', ht, '_ft_Flood_Stage/ImageServer/exportImage?',
#     'bbox=6217115,1919960,6364424,2011721&',
#     'adjustAspectRatio=false&',
#     'size=3000,3000&',
#     'imageSR=',
#       'PROJCS["NAD_1983_StatePlane_California_II_FIPS_0402_Feet",',
#         'GEOGCS["GCS_North_American_1983",',
#           'DATUM["D_North_American_1983",SPHEROID["GRS_1980",6378137,298.257222101]],',
#           'PRIMEM["Greenwich",0],',
#           'UNIT["Degree",0.017453292519943295]],',
#         'PROJECTION["Lambert_Conformal_Conic"],',
#         'PARAMETER["False_Easting",6561666.666666666],',
#         'PARAMETER["False_Northing",1640416.666666667],',
#         'PARAMETER["Central_Meridian",-122],',
#         'PARAMETER["Standard_Parallel_1",38.33333333333334],',
#         'PARAMETER["Standard_Parallel_2",39.83333333333334],',
#         'PARAMETER["Latitude_Of_Origin",37.66666666666666],','
#         UNIT["Foot_US",0.30480060960121924]]&',
#     'format=tiff&',
#     'f=image')
#   
#   ## save file download as raster
#   temp <- tempfile()
#   download.file(url, destfile = temp, mode = "wb")
#   flood <- raster(temp)
#   crs_ca <- proj4string(flood)
#   flood.repair <- flood %>%
#     raster.df %>%
#     rename(lat = y, long = x) %>%
#     rasterFromXYZ(crs = crs_ca)
#   unlink(temp)
#   assign(paste0('flood', ht), flood.repair)
# }
# 
# save(flood32, flood33, flood34, flood35, flood36, flood37, flood38, flood39,
#      flood40, flood41, flood42, flood43, flood44, flood45, flood46, flood47,
#      flood48, flood49, flood50, flood51, flood52,
#      file = '_data/flood_sonoma/flood_sonoma.Rdata')


#### open inundation scenario of interest #########################################################

flood_sonoma <- function(ht) {
  load('_data/flood_sonoma/flood_sonoma.Rdata')
  return(get(paste0('flood', ht)))
}

