
# ## validate G(INUN) floodplain
# https://sonomacounty.maps.arcgis.com/home/item.html?id=9d8d63558c6b4124b000e6476a0a020d
# https://data.sonomaopenspace.org/arcgis/rest/services/Projects

#### compare LISFLOOD vs. Sonoma County HEC-RAS flood heights

## find max flood crest at gage 11467002
# gage <- readNWISdata(
#   sites = 11467002, parameterCd = param,
#   startDate = ymd(catalog$start_day[year.id]) - days(1),
#   endDate = ymd(catalog$end_day[year.id]) + days(1),
#   service = 'iv', tz = 'America/Los_Angeles') %>%
#   renameNWISColumns
# max(gage$GH_Inst)

# for (ht in 32:52) {
# ## load Sonoma data
# url <- paste0(
#   'https://data.sonomaopenspace.org/arcgis/rest/services/Projects/',
#   'Guerneville_Gauge_at_', ht, '_ft_Flood_Stage/ImageServer/exportImage?',
#   # 'bbox=6236126.396389392,1919959.543629255,6341516.396389392,2075767.543629255&',
#   'bbox=6217115,1919960,6364424,2011721&',
#   'bbox=579500,1895000,616500,1935000&',
#   'adjustAspectRatio=false&',
#   'size=3000,3000&',
#   'imageSR=',
#     'PROJCS["NAD_1983_StatePlane_California_II_FIPS_0402_Feet",',
#       'GEOGCS["GCS_North_American_1983",',
#         'DATUM["D_North_American_1983",SPHEROID["GRS_1980",6378137,298.257222101]],',
#         'PRIMEM["Greenwich",0],',
#         'UNIT["Degree",0.017453292519943295]],',
#       'PROJECTION["Lambert_Conformal_Conic"],',
#       'PARAMETER["False_Easting",6561666.666666666],',
#       'PARAMETER["False_Northing",1640416.666666667],',
#       'PARAMETER["Central_Meridian",-122],',
#       'PARAMETER["Standard_Parallel_1",38.33333333333334],',
#       'PARAMETER["Standard_Parallel_2",39.83333333333334],',
#       'PARAMETER["Latitude_Of_Origin",37.66666666666666],','
#       UNIT["Foot_US",0.30480060960121924]]&',
#   'format=tiff&',
#   'f=image')
# 
# temp <- tempfile()
# download.file(url, destfile = temp, mode = "wb")
# flood <- raster(temp)
# crs_ca <- proj4string(flood)
# flood.repair <- flood %>%
#   raster.df %>%
#   # filter(value > 0) %>%
#   rename(lat = y, long = x) %>%
#   rasterFromXYZ(crs = crs_ca)
# unlink(temp)
# 
# ## use this to narrow down the bounding box request
# # prmd.extent <- extent(flood) %>%
# #   as('SpatialPolygons') %>%
# #   as('sf') %>%
# #   st_set_crs(crs_ca)
# # ext <- aoi %>%
# #   st_transform(crs_ca) %>%
# #   st_intersection(prmd.extent)
# # st_bbox(ext)
# 
# assign(paste0('flood', ht), flood.repair)
# }
# save(flood32, flood33, flood34, flood35, flood36, flood37, flood38, flood39,
#      flood40, flood41, flood42, flood43, flood44, flood45, flood46, flood47,
#      flood48, flood49, flood50, flood51, flood52,
#      file = 'C:/Users/cbowers/Desktop/flood_sonoma.Rdata')

