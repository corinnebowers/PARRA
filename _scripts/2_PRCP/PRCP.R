
###################################################################################################

fit_precip <- function(catalog, extremeratio = 0.1) { 
  #'
  #' Fits a weighted least squares linear regression to characterize the precipitation component 
  #' model, $f(PRCP|AR)$, for use in the PARRA framework. 
  #' 
  #' @param catalog Catalog of ARs occurring in area of interest. 
  #' Columns "IVT_max", "duration", and "precip_mm" must be present.
  #' @param extremeratio Fraction of AR events in the catalog to be used in fitting the 
  #' extremes component of the residuals mixture model. For example, a value of 0.1 means that 
  #' the standard deviation of the extremes is calculated based on the top 10% of AR events.
  #'
  #' @return "model.prcp", the fitted lm regression model object; and "se.prcp", the 
  #' fitted parameters for the residuals mixture model.
  #' 

  ## fit weighted linear regression to predict PRCP
  fun <- precip_mm ~ IVT_max*duration
  model.prcp <- lm(fun, data = catalog, weights = 1/(IVT_max*duration))
  
  ## save out regression object
  model.prcp <<- model.prcp
  
  ## calculate robust standard errors (sigma_i^2 = sigma^2 * IVT_max * duration)
  catalog <- catalog %>% 
    mutate(
      precip.star = precip_mm / sqrt(IVT_max*duration),
      c.star = 1 / sqrt(IVT_max*duration),
      IVT.star = IVT_max / sqrt(IVT_max*duration),
      dur.star = duration / sqrt(IVT_max*duration),
      interact.star = sqrt(IVT_max*duration))
  model.wls <- lm(
    precip.star ~ c.star + IVT.star + dur.star + interact.star + 0, data = catalog) 
  se.wls <- summary(model.wls)$sigma
  
  ## characterize distribution of extreme residuals
  extreme.id <- catalog %>% 
    mutate(n.AR = 1:nrow(.)) %>% 
    filter(duration >= quantile(duration, 1-extremeratio) & 
             IVT_max >= quantile(IVT_max, 1-extremeratio)) %>% pull(n.AR)
  extreme.resid <- model.prcp$residuals[extreme.id]
  se.extreme <- sd(extreme.resid)
  
  ## save out error model
  se.prcp <<- c('se.wls' = se.wls, 'se.extreme' = se.extreme)
}


###################################################################################################

generate_precip <- function(
  AR, model.prcp, se.prcp, mixratio = 0.9, probabilistic = FALSE, n.precip = 1) {
  #'
  #' Generates new realizations of precipitation based on IVT and duration.
  #' 
  #' @param AR Dataframe of ARs to generate precipitation for. Column "n.AR" must be present.
  #' @param model.prcp Fitted lm regression model object for precipitation.
  #' @param se.prcp Two-element vector of fitted parameters for the precipitation residuals 
  #' mixture model.
  #' @param mixratio Fraction of points to be pulled from the robust standard error 
  #' distribution. The remainder of errors in the mixture model will be pulled from the 
  #' distribution of extremes.
  #' @param probabilistic Logical binary that indicates whether to incorporate uncertainty into 
  #' new precipitation realizations.
  #' @param n.precip Number of precipitation realizations to generate per AR event.
  #' 
  #' @return a dataframe of synthetic precipitation realizations.
  #' 

  ## fix input parameters
  if (!probabilistic) n.precip <- 1
  
  ## calculate n.AR
  n.AR <- max(AR$n.AR)

  ## generate new precipitation realizations
  prediction.hetskd <- predict(model.prcp, AR) 
  if (probabilistic) { #add stochastic uncertainty to realizations
    precip <- 
      map_dfr(.x = 1:n.AR, 
        .f = function(i) {
          error.hetskd <- 
            rnorm(n.precip, sd = se.prcp[1]) * sqrt(AR$IVT_max[i]*AR$duration[i])
          error.extreme <- rnorm(n.precip, sd = se.prcp[2])
          error.mixture <- cbind(error.hetskd, error.extreme) %>% 
            as.data.frame %>% 
            mutate(
              id = sample(c(0,1), size = n.precip, replace = TRUE, 
                          prob = c(mixratio, 1-mixratio)),
              error.mix = case_when(id==0 ~ error.hetskd, id==1 ~ error.extreme)) %>% 
            pull(error.mix)
          data.frame(
            n.AR = i, n.precip = 1:n.precip, 
            precip_mm = (prediction.hetskd[i] + error.mixture))
          })
    
  } else { #report deterministic expected value
    precip <- AR %>% transmute(n.AR, n.precip = NA, precip_mm = prediction.hetskd)
  }

  ## return precipitation dataframe
  precip <- precip %>% 
    mutate(precip_mm = case_when(precip_mm < 0 ~ 0, TRUE ~ precip_mm)) %>% 
    full_join(AR, ., by = 'n.AR')
  return(precip)
}


###################################################################################################