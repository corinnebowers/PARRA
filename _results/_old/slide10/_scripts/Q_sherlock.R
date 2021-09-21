
###################################################################################################


## @param
## precip (data.frame): list of synthetic precipitation events
 # cols = n.AR, n.precip, n.hc, IVT_max, duration, precip_mm, sm, p
## catalog (data.frame): catalog of ARs occurring in region of interest
 # cols = IVT_max, duration, precip_mm, runoff_mm, wy
## probabilistic (logical): choice to incorporate uncertainty
## n.runoff (integer): number of runoff events to generate per precipitation event
## boot (integer): number of bootstrap samples to draw 

## @return
## runoff (data.frame): list of synthetic runoff events
 # cols = n.AR, n.precip, n.hc, n.runoff, precip_mm, runoff_mm, sm

generate_runoff <- function(
  precip, catalog, probabilistic = FALSE, n.runoff = 1, boot = 1e3) {
  
  ## fix input parameters
  if (!probabilistic) n.runoff <- 1

  ## calculate probabilistic counters
  n.AR <- max(precip$n.AR)
  if (any(is.na(precip$n.precip))) {
    prcp.memory <- TRUE
    precip$n.precip <- 1
  } else prcp.memory <- FALSE
  n.precip <- max(precip$n.precip)
  if (any(is.na(precip$n.hc))) {
    hc.memory <- TRUE
    precip$n.hc <- 1
  } else hc.memory <- FALSE
  n.hc <- max(precip$n.hc)
  
  print('converting precipitation to runoff...')

  ## set up runoff dataframe
  runoff <- expand.grid(
    n.AR = 1:n.AR, n.precip = 1:n.precip, n.hc = 1:n.hc, n.runoff = 1:n.runoff) %>% 
    right_join(precip, by = c('n.AR', 'n.precip', 'n.hc')) %>% 
    mutate(precip_in = precip_mm/25.4) 
  
  ## find the characteristics of the annual max storms
  wateryear.df <- 
    data.frame(wateryear = unique(catalog$wy), precip_in = NA, runoff_in = NA)
  for (i in 1:nrow(wateryear.df)) {
    index <- catalog %>% subset(wy == unique(wy)[i]) %>% 
      pull(precip_mm) %>% which.max
    index.value <- catalog %>% subset(wy == unique(wy)[i]) %>% 
      subset(1:nrow(.) == index)
    wateryear.df[i, 'precip_in'] <- index.value$precip_mm/25.4
    wateryear.df[i, 'runoff_in'] <- index.value$runoff_mm/25.4
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
      mutate(S = qnorm(p = p, mean = S.mean, sd = S.sd), 
             CN = 1000/(10+exp(S)), n.runoff = NA)
  }
  
  ## calculate expected runoff
  runoff <- runoff %>%
    mutate(
      runoff_in = ifelse(
        precip_in < 0.2*exp(S), 0, (precip_in-0.2*exp(S))^2/(precip_in+0.8*exp(S))),
      runoff_mm = runoff_in*25.4) %>% 
    select(n.AR, n.precip, n.hc, n.runoff, IVT_max, duration, precip_mm, runoff_mm, sm)
  if (prcp.memory) runoff$n.precip <- NA
  if (hc.memory) runoff$n.hc <- NA
  
  ## return runoff
  return(runoff)
}


###################################################################################################

## @param
## runoff (data.frame): list of synthetic runoff events
 # cols = n.AR, n.precip, n.hc, n.runoff, precip_mm, runoff_mm, sm
## catalog (data.frame): catalog of ARs occurring in region of interest
 # cols = precip_mm, runoff_mm, Qp_m3s, tp_hrs
## probabilistic (logical): choice to incorporate uncertainty
## n.hydro (integer): number of hydrographs to generate per runoff event

## @return
## hydrograph (data.frame): list of synthetic hydrographs
 # cols = n.AR, n.precip, n.hc, n.runoff, n.hydro, Qp_m3s, tp_hrs

generate_hydrograph <- function(
  runoff, catalog, probabilistic = FALSE, n.hydro = 1) {

  ## fix input parameters
  if (!probabilistic) n.hydro <- 1

  ## calculate probabilistic counters
  n.AR <- max(runoff$n.AR)
  if (any(is.na(runoff$n.precip))) {
    prcp.memory <- TRUE
    runoff$n.precip <- 1
  } else prcp.memory <- FALSE
  n.precip <- max(runoff$n.precip)
  if (any(is.na(runoff$n.hc))) {
    hc.memory <- TRUE
    runoff$n.hc <- 1
  } else hc.memory <- FALSE
  n.hc <- max(runoff$n.hc)
  if (any(is.na(runoff$n.runoff))) {
    rnff.memory <- TRUE
    runoff$n.runoff <- 1
  } else rnff.memory <- FALSE
  n.runoff <- max(runoff$n.runoff)

  print('converting runoff to streamflow...')
  
  ## fit linear regression to predict Qp
  fun <- Qp ~ runoff*precip
  model <- lm(fun, data = catalog %>% rename(Qp = Qp_m3s, runoff = runoff_mm, precip = precip_mm))

  ## find prediction uncertainty
  predictions <- 
    predict(model, runoff %>% rename(runoff = runoff_mm, precip = precip_mm)) %>% 
    as.data.frame %>% setNames('fit') %>% 
    mutate(se.predict = predict.se(model, 
      fitdata = catalog %>% 
        rename(Qp = Qp_m3s, runoff = runoff_mm, precip = precip_mm),
      newdata = runoff %>% 
        rename(runoff = runoff_mm, precip = precip_mm))) %>% 
    cbind(runoff %>% select(n.AR, n.precip, n.runoff))
  
  ## generate realizations of Qp
  if (probabilistic) {
    hydrograph <- 
      expand.grid(
        n.AR = 1:n.AR, n.precip = 1:n.precip, n.hc = 1:n.hc, 
        n.runoff = 1:n.runoff, n.hydro = 1:n.hydro) %>% 
      right_join(runoff %>% select(n.AR, n.precip, n.runoff), 
                 by = c('n.AR', 'n.precip', 'n.runoff')) %>% 
      left_join(predictions, by = c('n.AR', 'n.precip', 'n.runoff')) %>% 
      mutate(Qp_m3s = rnorm(nrow(.), mean = fit, sd = se.predict))
  } else {
    hydrograph <- predictions %>% mutate(n.hydro = NA) %>% rename(Qp_m3s = fit)
  }
  hydrograph <- hydrograph %>% mutate(Qp_m3s = ifelse(Qp_m3s < 0, 0, Qp_m3s))
  
  ## generate realizations of tp
  fit.tp <- fitdist(catalog$tp_hrs[!is.na(catalog$tp_hrs)], 'lnorm')$estimate
  if (probabilistic) {
    hydrograph <- hydrograph %>% 
      mutate(tp_hrs = rlnorm(nrow(.), meanlog = fit.tp[1], sdlog = fit.tp[2]))
  } else {
    hydrograph <- hydrograph %>% mutate(tp_hrs = exp(fit.tp[1]))
  }
  
  ## return fitted model objects
  fit.tp <<- fit.tp
  model.Qp <<- model
  
  ## return hydrograph
  hydrograph <- hydrograph %>% 
    right_join(runoff, by = c('n.AR', 'n.precip', 'n.hc', 'n.runoff')) %>% 
    select(n.AR, n.precip, n.hc, n.runoff, n.hydro, IVT_max, duration, 
           precip_mm, runoff_mm, sm, Qp_m3s, tp_hrs)
  if (prcp.memory) hydrograph$n.precip <- rep(NA, nrow(hydrograph))
  if (hc.memory) hydrograph$n.hc <- rep(NA, nrow(hydrograph))
  if (rnff.memory) hydrograph$n.runoff <- rep(NA, nrow(hydrograph))
  return(hydrograph)
}
