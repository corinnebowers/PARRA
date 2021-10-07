
###################################################################################################

## @param
## AR (data.frame): list of synthetic ARs
 # cols = n.AR, IVT_max, duration
## catalog (data.frame): catalog of ARs occurring in region of interest
 # cols = IVT_max, duration, precip_mm
## probabilistic (logical): choice to incorporate uncertainty
## n.precip (integer): number of precipitation events to generate per AR
## extremeratio (double): fraction of points used to estimate distribution of extremes
## mixratio (double): fraction of points to be pulled from the robust standard error distribution 
 # (remainder of errors are pulled from distribution of extremes)

## @return
## precip (data.frame): list of synthetic precipitation events

generate_precip <- function(
  AR, catalog, probabilistic = FALSE, n.precip = 1, 
  extremeratio = 0.9, mixratio = 0.9, se.mult = 2) {

  ## fix input parameters
  if (!probabilistic) n.precip <- 1
  
  ## calculate n.AR
  n.AR <- max(AR$n.AR)

  ## fit weighted linear regression to predict PRCP
  fun <- precip_mm ~ IVT_max*duration
  model.wls <- lm(fun, data = catalog, weights = 1/(IVT_max*duration))
  prediction.hetskd <- predict(model.wls, AR)
  
  ## calculate robust standard errors
  ## sigma_i^2 = sigma^2 * IVT_max * duration
  catalog <- catalog %>% 
    mutate(precip.star = precip_mm / sqrt(IVT_max*duration),
           c.star = 1 / sqrt(IVT_max*duration),
           IVT.star = IVT_max / sqrt(IVT_max*duration),
           dur.star = duration / sqrt(IVT_max*duration),
           interact.star = sqrt(IVT_max*duration))
  model.hetskd <- lm(
    precip.star ~ c.star + IVT.star + dur.star + interact.star + 0, data = catalog) 
  se.hetskd <- summary(model.hetskd)$sigma
  
  ## characterize distribution of extreme residuals
  extreme.id <- catalog %>% 
    mutate(n.AR = 1:nrow(.)) %>% 
    filter(duration >= quantile(duration, extremeratio) & 
             IVT_max >= quantile(IVT_max, extremeratio)) %>% pull(n.AR)
  extreme.resid <- model.wls$residuals[extreme.id]
  
  ## generate new precipitation realizations
  if (probabilistic) {
    precip <- 
      map_dfr(.x = 1:n.AR, 
        .f = function(i) {
          error.hetskd <- rnorm(n.precip, sd = se.hetskd) * 
            sqrt(AR$IVT_max[i]*AR$duration[i])
          error.extreme <- rnorm(n.precip, sd = sd(extreme.resid))
          error.mixture <- 
            cbind(error.hetskd, error.extreme) %>% as.data.frame %>% 
            mutate(id = sample(c(0,1), size = n.precip, replace = TRUE, 
              prob = c(mixratio, 1-mixratio))) %>% 
            mutate(error.mix = case_when(id==0 ~ error.hetskd, id==1 ~ error.extreme)) %>% 
            pull(error.mix)
          data.frame(
            n.AR = i, n.precip = 1:n.precip, 
            precip_mm = (prediction.hetskd[i] + error.mixture*se.mult) %>% 
              cbind(0) %>% apply(1, max))
          })
  } else {
    precip <- AR %>% transmute(n.AR, n.precip = NA, precip_mm = prediction.hetskd)
  }
  
  ## return fitted model object
  model.prcp <<- model.wls
  
  ## return precipitation dataframe
  precip <- precip %>% 
    mutate(precip_mm = case_when(precip_mm < 0 ~ 0, TRUE ~ precip_mm)) %>% 
    full_join(AR, ., by = 'n.AR')
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
  
  ## update probabilistic counters
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
        mutate(sm = rlnorm(nrow(.), meanlog = fit.sm[1], sdlog = fit.sm[2]))
    } else precip.hc <- precip %>% mutate(sm = exp(fit.sm[1]), n.hc = NA)
    if (prcp.memory) precip.hc$n.precip <- NA
    
  } else {
    precip.hc <- precip %>% mutate(n.hc = NA)
  }
  
  ## return precipitation dataframe with simulated soil moisture
  return(precip.hc)
}


###################################################################################################

