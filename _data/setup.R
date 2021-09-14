
###################################################################################################
## setup.R: loads packages & custom functions for analysis
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
  require(foreach)
  require(doSNOW)
  require(parallel)
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
  return(se.pred*2)
}


###################################################################################################

