
###################################################################################################
## setup.R: loads packages & custom functions
## Corinne Bowers
## 7/1/2021
###################################################################################################

## packages
suppressPackageStartupMessages({
  require(sf)
  require(terra)
  require(raster)
  require(tigris); options(tigris_use_cache = TRUE)
  require(censusapi); Sys.setenv(CENSUS_KEY = 'f2e090156b02ced027d4ed756f82c9a3a1aa38c9')
  require(lubridate)
  require(ncdf4)
  require(rnoaa); rnoaa_options(cache_messages = FALSE)
  require(mvtnorm)
  require(evd)
  require(quantreg)
  require(caret)
  require(pracma)
  require(dataRetrieval)
  require(exactextractr)
  require(fitdistrplus)
  require(scales) 
  require(foreach)
  require(doSNOW)
  require(parallel)
  require(ggspatial)
  require(cowplot)
  require(ggridges)
  require(leaflet)
  require(mapboxapi)
  require(scico)
  require(extrafont)
  require(tidyverse)
})


###################################################################################################

## helper functions
mft <- 3.28084

toNumber <- function(x) as.numeric(paste(x))
Mean <- function(x) mean(x, na.rm = TRUE)
Sum <- function(x) sum(x, na.rm = TRUE)
Max <- function(x) max(x, na.rm = TRUE)
Min <- function(x) min(x, na.rm = TRUE)
sum.na <- function(x) sum(is.na(x))

wateryear <- function(d) year(d) + ifelse(month(d) %in% 10:12, 1, 0)

predict.se <- function(model, fitdata, newdata) {
  dof <- nrow(fitdata) - ncol(model.matrix(model, fitdata)) #find degrees of freedom
  MSE <- sqrt(sum(residuals(model)^2)/dof) #find MSE
  V <- solve(t(model.matrix(model, fitdata)) %*% 
               model.matrix(model, fitdata)) * MSE^2 #find var-cov matrix of coefficients
  X <- model.matrix(delete.response(terms(model)), newdata) #create matrix of new data
  yhat <- predict(model, newdata) #predict new response
  var.fit <- rowSums((X %*% V) * X) #find the diagonal of the var-cov matrix for yhat
  # se.conf <- sqrt(var.fit) #find pointwise standard errors of predicted mean (CI)
  se.pred <- sqrt(var.fit + MSE^2)*2 #find standard error of the prediction interval (PI)
  return(se.pred)
}


###################################################################################################

## plotting functions
raster.df <- function(x) x %>% as.data.frame(xy = TRUE) %>% setNames(c('x', 'y', 'value'))

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

## geometry: Sonoma County census tracts
sonoma <- tracts(state = 'CA', county = 'Sonoma') %>% 
  subset(NAME != 9901) %>% 
  st_transform(6417)

## geometry: Russian River
# https://data.ca.gov/dataset/national-hydrography-dataset-nhd
russian <- st_read('D:/1-PARRA/_data/NHD/MajorRivers.shp', quiet = TRUE) %>% 
  st_zm(st_transform(6417)) %>% 
  subset(grepl('Russian', GNIS_Name))

## colors
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


###################################################################################################

