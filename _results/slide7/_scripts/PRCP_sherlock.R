
###################################################################################################

## @param
## AR (data.frame): list of synthetic ARs
 # cols = n.AR, IVT_max, duration
## catalog (data.frame): catalog of ARs occurring in region of interest
 # cols = IVT_max, duration, precip_mm
## probabilistic (logical): choice to incorporate uncertainty
## n.precip (integer): number of precipitation events to generate per AR

## @return
## precip (data.frame): list of synthetic precipitation events

generate_precip <- function(AR, catalog, probabilistic = FALSE, n.precip = 1) {

  ## fix input parameters
  if (!probabilistic) n.precip <- 1
  
  ## calculate n.AR
  n.AR <- max(AR$n.AR)

  ## fit quantile regression model
  fun <- precip ~ IVT_max*duration
  temp <-
    foreach (t = seq(0, 1, 0.01), .combine = 'rbind') %do% {
      model <- rq(fun, data = catalog %>% rename(precip = precip_mm), tau = t)
      prediction <- predict(model)
      prediction <- ifelse(prediction < 0, 0, prediction)
      c(tau = t, RMSE = RMSE(sort(prediction), sort(catalog$precip_mm)))
    }
  tau.best <- temp %>% as.data.frame %>% .[which.min(.$RMSE), 'tau']
  model <- rq(fun, data = catalog %>% rename(precip = precip_mm), tau = tau.best)
  AR <- AR %>%
    mutate(precip.mean = predict.rq(model, AR),
           precip.sd = predict.se(model, catalog %>% rename(precip = precip_mm), AR))
  
  ## generate precipitation realizations
  if (probabilistic) {
    precip <- 
      map_dfr(.x = 1:n.AR, 
        .f = function(i) {
          rnorm(n = n.precip, mean = AR$precip.mean[i], sd = AR$precip.sd[i]) %>% 
            data.frame(n.AR = i, n.precip = 1:n.precip, precip_mm = .)
        })
  } else {
    precip <- AR %>% transmute(n.AR, n.precip = NA, precip_mm = precip.mean)
  }
  
  ## return fitted model object
  model.prcp <<- model
  
  ## return precipitation dataframe
  precip <- precip %>% 
    mutate(precip_mm = case_when(precip_mm < 0 ~ 0, TRUE ~ precip_mm)) %>% 
    full_join(AR %>% select(-precip.mean, -precip.sd), ., by = 'n.AR')
  return(precip)
}


###################################################################################################

## @param
## precip (data.frame): list of synthetic precipitation events
# cols = n.AR, n.precip, sm*
# *NOTE: column 'sm' must be present if you want to use observed data, otherwise
# this function will generate new values
## catalog (data.frame): catalog of ARs occurring in region of interest
# cols = sm
## probabilistic (logical): choice to incorporate uncertainty
## n.hc (integer): number of soil moisture realizations to generate per precipitation event

## @return
## precip (data.frame): list of synthetic precipitation events
# cols = n.AR, n.precip, n.hc, sm, p

generate_soilmoisture <- function(
  precip, catalog, probabilistic = FALSE, n.hc = 1) {
  
  ## fix input parameters
  if (!probabilistic) n.hc <- 1
  
  ## calculate probabilistic counters
  n.AR <- max(precip$n.AR)
  if (any(is.na(precip$n.precip))) {
    prcp.memory <- TRUE
    precip$n.precip <- 1
  } else prcp.memory <- FALSE
  n.precip <- max(precip$n.precip)
  
  ## fit a lognormal distribution to soil moisture values in historic catalog
  fit.sm <- fitdist(catalog$sm, 'lnorm')$estimate
  
  ## return fitted model object
  fit.sm <<- fit.sm
  
  ## check if observed soil moisture records already exist
  if (!('sm' %in% names(precip))) {  
    
    ## randomly generate new soil moisture records
    if (probabilistic) {
      precip.hc <-
        expand.grid(n.AR = 1:n.AR, n.precip = 1:n.precip, n.hc = 1:n.hc) %>% 
        right_join(precip, by = c('n.AR', 'n.precip')) %>%
        mutate(sm = rlnorm(nrow(.), meanlog = fit.sm[1], sdlog = fit.sm[2])) %>% 
        mutate(p = plnorm(sm, meanlog = fit.sm[1], sdlog = fit.sm[2])) %>% 
        mutate(p = case_when(p == 0 ~ 1e-6, p == 1 ~ 1-1e-6, TRUE ~ p))
    } else {
      precip.hc <- precip %>% mutate(sm = exp(fit.sm[1]), p = 0.5, n.hc = NA)
    }
    if (prcp.memory) precip.hc$n.precip <- NA
    
  } else {
    precip.hc <- precip %>% 
      mutate(p = plnorm(sm, meanlog = fit.sm[1], sdlog = fit.sm[2]), n.hc = NA)
  }
  
  ## return precipitation dataframe with simulated soil moisture
  return(precip.hc)
}


###################################################################################################

