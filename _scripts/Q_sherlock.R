
###################################################################################################

## @param
## precip (data.frame): list of synthetic precipitation events
## catalog (data.frame): catalog of ARs occurring in region of interest
## probabilistic (logical): choice to incorporate uncertainty
## n.hc (integer): number of soil moisture realizations to generate per precipitation event

## @return
## precip (data.frame): list of synthetic precipitation events

generate_soilmoisture <- function(
  precip, catalog, probabilistic = FALSE, n.hc = 1) {
  
  ## fix input parameters
  if (!probabilistic) n.hc <- 1
  
  ## calculate n.AR and n.precip
  n.AR <- max(precip$n.AR)
  if (any(is.na(precip$n.precip))) {
    prcp.memory <- TRUE
    precip$n.precip <- 1
  } else prcp.memory <- FALSE
  n.precip <- max(precip$n.precip)

  if (!('sm' %in% names(precip))) {  
    ## fit a lognormal distribution to soil moisture values in historic catalog
    fit.sm <- fitdist(catalog$sm, 'lnorm')$estimate
    
    ## randomly generate new soil moisture records
    ## (note: this is always going to be probabilistic, not really a deterministic option)
    precip <- 
      expand.grid(n.AR = 1:n.AR, n.precip = 1:n.precip, n.hc = 1:n.hc) %>% 
      right_join(precip, by = c('n.AR', 'n.precip')) %>% 
      mutate(sm = rlnorm(nrow(.), meanlog = fit.sm[1], sdlog = fit.sm[2])) %>% 
      mutate(p = 1 - plnorm(sm, meanlog = fit.sm[1], sdlog = fit.sm[2])) %>% 
      mutate(p = case_when(p == 0 ~ 1e-6, p == 1 ~ 1-1e-6, TRUE ~ p))
    if (prcp.memory) precip$n.precip <- NA
    fit.sm <<- fit.sm
  }
  return(precip)
}


###################################################################################################

## @param
## precip (data.frame): list of synthetic precipitation events
## catalog (data.frame): catalog of ARs occurring in region of interest
## probabilistic (logical): choice to incorporate uncertainty
## n.runoff (integer): number of runoff events to generate per precipitation event
## boot (integer): number of bootstrap samples to draw 

## @return
## runoff (data.frame): list of synthetic runoff events

generate_runoff <- function(
  precip, catalog, probabilistic = FALSE, n.runoff = 1, boot = 1e3) {
  
  ## fix input parameters
  if (!probabilistic) n.runoff <- 1

  ## calculate n.AR and n.precip
  n.AR <- max(precip$n.AR)
  if (any(is.na(precip$n.precip))) {
    prcp.memory <- TRUE
    precip$n.precip <- 1
  } else prcp.memory <- FALSE
  n.precip <- max(precip$n.precip)

  print('converting precipitation to runoff...')

  ## set up runoff dataframe
  runoff <- expand.grid(n.AR = 1:n.AR, n.precip = 1:n.precip, n.runoff = 1:n.runoff) %>% 
    right_join(precip, by = c('n.AR', 'n.precip')) %>% 
    mutate(precip_in = precip_mm/25.4) 
  
  ## find the characteristics of the annual max storms
  wateryear.df <- 
    data.frame(wateryear = unique(catalog$wy), precip_in = NA, runoff_in = NA)
  for (i in 1:nrow(wateryear.df)) {
    index <- catalog %>% subset(wy == unique(wy)[i]) %>% 
      pull(precip_obs) %>% which.max
    index.value <- catalog %>% subset(wy == unique(wy)[i]) %>% 
      subset(1:nrow(.) == index)
    wateryear.df[i, 'precip_in'] <- index.value$precip_obs/25.4
    wateryear.df[i, 'runoff_in'] <- index.value$runoff_obs/25.4
    wateryear.df[i, 'IVT_max'] <- index.value$IVT_max
    wateryear.df[i, 'duration'] <- index.value$duration
  }
  
  ## fit the curve number function based on annual max storms
  if (probabilistic) {
    ## bootstrap confidence intervals for CN parameters
    S.best <- rep(NA, boot)
    S.sd <- rep(NA, boot)
    for (b in 1:boot) {
      index <- sample(1:nrow(wateryear.df), size = nrow(wateryear.df), replace = TRUE)
      P <- wateryear.df$precip_in[index]
      R <- wateryear.df$runoff_in[index]
      S.val <- ifelse(P>0 & R>0, 5*(P + 2*R - sqrt(5*P*R + 4*R^2)), NA)
      S.best[b] <- mean(log(S.val), na.rm = TRUE)
      S.sd[b] <- sd(log(S.val), na.rm = TRUE)
    }
    ## sample a CN value from the normal distribution
    runoff <- runoff %>% 
      mutate(S.mean = rnorm(nrow(.), mean = mean(S.best), sd = sd(S.best)),
             S.sd = rnorm(nrow(.), mean = mean(S.sd), sd = sd(S.sd)),
             S = qnorm(p = 1-p, mean = S.mean, sd = S.sd),
             CN = 1000/(10+exp(S)))
    
  } else {
    ## calculate CN deterministically
    P <- wateryear.df$precip_in
    R <- wateryear.df$runoff_in
    S.val <- ifelse(P>0 & R>0, 5*(P + 2*R - sqrt(5*P*R + 4*R^2)), NA)
    S.mean <- mean(log(S.val), na.rm = TRUE)
    S.sd <- sd(log(S.val), na.rm = TRUE)
    runoff <- runoff %>% 
      mutate(S = qnorm(p = 1-p, mean = S.mean, sd = S.sd), 
             CN = 1000/(10+exp(S)), n.runoff = NA)
  }
  
  ## calculate expected runoff
  runoff <- runoff %>%
    mutate(runoff_in = ifelse(precip_in < 0.2*exp(S), 0, 
                              (precip_in-0.2*exp(S))^2/(precip_in+0.8*exp(S))),
           runoff_mm = runoff_in*25.4) %>% 
    select(n.AR, n.precip, n.runoff, IVT_max, duration, precip_mm, runoff_mm, sm) %>% 
    rename(runoff_sim = runoff_mm)
  if (prcp.memory) runoff$n.precip <- NA
  return(runoff)
}


###################################################################################################

## @param
## runoff (data.frame): list of synthetic runoff events
## catalog (data.frame): catalog of ARs occurring in region of interest
## probabilistic (logical): choice to incorporate uncertainty
## n.hydro (integer): number of hydrographs to generate per runoff event

## @return
## hydrograph (data.frame): list of synthetic hydrographs

generate_hydrograph <- function(
  precip, runoff, catalog, probabilistic = FALSE, n.hydro = 1) {

  ## fix input parameters
  if (!probabilistic) n.hydro <- 1

  ## calculate n.AR, n.precip, n.runoff
  n.AR <- max(runoff$n.AR)
  if (any(is.na(precip$n.precip))) {
    prcp.memory <- TRUE
    precip$n.precip <- 1
    runoff$n.precip <- rep(1, nrow(runoff))
  } else prcp.memory <- FALSE
  n.precip <- max(precip$n.precip)
  if (any(is.na(runoff$n.runoff))) {
    rnff.memory <- TRUE
    runoff$n.runoff <- 1
  } else rnff.memory <- FALSE
  n.runoff <- max(runoff$n.runoff)

  print('converting runoff to streamflow...')
  
  ## fit linear regression to predict Qp
  fun <- Qp ~ runoff*precip
  model <- lm(fun, data = catalog %>% rename(Qp = Qp_obs, runoff = runoff_obs, precip = precip_obs))

  ## find prediction uncertainty
  predictions <- predict(model, runoff %>% rename(runoff = runoff_sim, precip = precip_mm)) %>% 
    as.data.frame %>% setNames('fit') %>% 
    mutate(se.predict = predict.se(model, 
      fitdata = catalog %>% rename(Qp = Qp_obs, runoff = runoff_obs, precip = precip_obs),
      newdata = runoff %>% rename(runoff = runoff_sim, precip = precip_mm))) %>% 
    cbind(runoff %>% select(n.AR, n.precip, n.runoff))
  
  ## generate realizations of Qp
  if (probabilistic) {
    hydrograph <- 
      expand.grid(n.AR = 1:n.AR, 
                  n.precip = 1:n.precip,
                  n.runoff = 1:n.runoff, 
                  n.hydro = 1:n.hydro) %>% 
      right_join(runoff %>% select(n.AR, n.precip, n.runoff), 
                 by = c('n.AR', 'n.precip', 'n.runoff')) %>% 
      left_join(predictions, by = c('n.AR', 'n.precip', 'n.runoff')) %>% 
      mutate(Qp_sim = rnorm(nrow(.), mean = fit, sd = se.predict))
  } else {
    hydrograph <- predictions %>% mutate(n.hydro = NA) %>% rename(Qp_sim = fit)
  }
  hydrograph <- hydrograph %>% mutate(Qp_sim = ifelse(Qp_sim < 0, 0, Qp_sim))
  
  ## generate realizations of tp
  fit.tp <- fitdist(catalog$tp_hrs[!is.na(catalog$tp_hrs)], 'lnorm')$estimate
  hydrograph <- hydrograph %>% 
    mutate(tp_sim = rlnorm(nrow(.), meanlog = fit.tp['meanlog'], sdlog = fit.tp['sdlog']))
  
  ## return hydrograph
  fit.tp <<- fit.tp
  model.Qp <<- model
  hydrograph <- hydrograph %>% 
    right_join(runoff, by = c('n.AR', 'n.precip', 'n.runoff')) %>% 
    select(n.AR, n.precip, n.runoff, n.hydro, IVT_max, duration, 
           precip_mm, runoff_sim, sm, Qp_sim, tp_sim)
  if (prcp.memory) {
    runoff$n.precip <- rep(NA, nrow(runoff))
    hydrograph$n.precip <- rep(NA, nrow(hydrograph))
  }
  if (rnff.memory) hydrograph$n.runoff <- rep(NA, nrow(hydrograph))
  return(hydrograph)
}
