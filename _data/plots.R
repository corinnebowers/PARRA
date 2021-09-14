
###################################################################################################
## plots.R: loads packages & custom functions for figures
## Corinne Bowers
## 9/11/2021
###################################################################################################

## packages
suppressPackageStartupMessages({
  require(scales) 
  require(ggspatial)
  require(cowplot)
  require(ggridges)
  require(leaflet)
  require(mapboxapi)
  require(scico)
  require(extrafont)
  require(mapview)
  require(grid)
  require(gridExtra)
  require(ggpubr)
  require(gt)
})

#### themes & axes ################################################################################

scale_x_origin <- function(...) {
  scale_x_continuous(expand = expansion(mult = c(0, 0.01)), limits = c(0,NA), ...) }
scale_y_origin <- function(...) {
  scale_y_continuous(expand = expansion(mult = c(0, 0.01)), limits = c(0,NA), ...) }
geom_parity <- function() geom_abline(slope = 1, intercept = 0, linetype = 'dashed')

theme_bw_custom <- 
  function() {
    theme_bw() + theme(
      text = element_text(family = 'Segoe UI', size = 8),
      panel.border = element_rect(fill = NA, color = 'black', size = 0.35),
      axis.ticks = element_line(size = 0.35, color = 'black'),
      legend.key.size = unit(0.35, 'cm'))}

theme_set(
  theme_classic() + theme(
    text = element_text(family = 'Segoe UI', size = 8),
    axis.line = element_line(size = 0.35),
    axis.ticks = element_line(size = 0.35, color = 'black'),
    legend.key.size = unit(0.35, 'cm')))


#### colors #######################################################################################

baker <- c()
baker[1] <- rgb(56, 95, 150, maxColorValue = 255)
baker[2] <- rgb(207, 89, 33, maxColorValue = 255)
baker[3] <- rgb(158, 184, 219, maxColorValue = 255)
baker[4] <- rgb(231, 184, 0, maxColorValue = 255)
baker[5] <- rgb(128, 0, 0, maxColorValue = 255)

ggcolor <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


#### geospatial ###################################################################################

## to plot rasters in ggplot environment
raster.df <- function(x) x %>% as.data.frame(xy = TRUE) %>% setNames(c('x', 'y', 'value'))

## geometry: Russian River
# https://data.ca.gov/dataset/national-hydrography-dataset-nhd
russian <- st_read('_data/NHD/MajorRivers.shp', quiet = TRUE) %>% 
  st_zm(st_transform(6417)) %>% 
  subset(grepl('Russian', GNIS_Name))

## geometry: Sonoma County census tracts
sonoma <- tracts(state = 'CA', county = 'Sonoma') %>% 
  subset(NAME != 9901) %>% 
  st_transform(6417)


###################################################################################################
