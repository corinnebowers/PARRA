
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
  precip, catalog, probabilistic = FALSE, n.runoff = 1, boot = 1e3, se.mult = 2) {
  
  ## fix input parameters
  if (!probabilistic) n.runoff <- 1

  ## update probabilistic counters
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
    boot.mean <- rep(NA, boot)
    boot.sd <- rep(NA, boot)
    for (b in 1:boot) {
      index <- sample(1:nrow(wateryear.df), size = nrow(wateryear.df), replace = TRUE)
      P <- wateryear.df$precip_in[index]
      R <- wateryear.df$runoff_in[index]
      S.val <- ifelse(P>0 & R>0, 5*(P + 2*R - sqrt(5*P*R + 4*R^2)), NA)
      boot.mean[b] <- mean(S.val, na.rm = TRUE)
      boot.sd[b] <- sd(S.val, na.rm = TRUE)
    }
    ## sample a CN value from the bootstrapped distribution
    runoff <- runoff %>% mutate(
      S.mean = rnorm(nrow(.), mean(boot.mean), sd(boot.mean)),
      S.sd = rnorm(nrow(.), mean(boot.sd), sd(boot.sd)),
      S = qnorm(p = (sm-min(sm)+0.5)/(max(sm)-min(sm)+1), mean = S.mean, sd = S.sd*se.mult),
      CN = 1000/(10+S))
    
  } else {
    ## calculate CN deterministically
    P <- wateryear.df$precip_in
    R <- wateryear.df$runoff_in
    S.val <- ifelse(P>0 & R>0, 5*(P + 2*R - sqrt(5*P*R + 4*R^2)), NA)
    S.mean <- mean(S.val, na.rm = TRUE)
    S.sd <- sd(S.val, na.rm = TRUE)
    runoff <- runoff %>% mutate(
      S = qnorm(p = (sm-min(sm)+0.5)/(max(sm)-min(sm)+1), mean = S.mean, sd = S.sd), 
      CN = 1000/(10+S), 
      n.runoff = NA)
  }
  
  ## calculate expected runoff
  runoff <- runoff %>% 
    mutate(
      runoff_in = case_when(
        precip_in < 0.2*(S) ~ 0, 
        TRUE ~ (precip_in-0.2*S)^2/(precip_in+0.8*S)),
      runoff_mm = runoff_in*25.4) %>% 
    mutate(
      runoff_mm = runoff_mm %>% 
        cbind(precip_mm) %>% apply(1, min) %>% 
        cbind(0) %>% apply(1, max)) %>% 
    select(n.AR, n.precip, n.hc, n.runoff, IVT_max, duration, precip_mm, runoff_mm, sm)
  if (prcp.memory) runoff$n.precip <- NA
  if (hc.memory) runoff$n.hc <- NA
  
  ## return runoff dataframe
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
## mixratio (double): fraction of points to be pulled from the OLS error distribution 
 # (remainder of errors are pulled from distribution of extremes)
## extremeratio (double): fraction of points used to estimate distribution of extremes

## @return
## hydrograph (data.frame): list of synthetic hydrographs
 # cols = n.AR, n.precip, n.hc, n.runoff, n.hydro, Qp_m3s, tp_hrs

generate_hydrograph <- function(
  runoff, catalog, probabilistic = FALSE, n.hydro = 1, 
  extremeratio = 0.9, mixratio = 0.9, se.mult = 2) {

  ## fix input parameters
  if (!probabilistic) n.hydro <- 1

  ## update probabilistic counters
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
  model <- lm(Qp_m3s ~ precip_mm*runoff_mm, data = catalog)
  runoff$prediction <- predict(model, runoff)

  ## characterize distribution of extreme residuals
  extreme.id <- catalog %>% 
    mutate(n.AR = 1:nrow(.)) %>% 
    filter(precip_mm >= quantile(precip_mm, extremeratio) & 
             runoff_mm >= quantile(runoff_mm, extremeratio)) %>% pull(n.AR)
  extreme.resid <- model$residuals[extreme.id]

  ## generate new realizations of Qp
  if (probabilistic) {
    hydrograph <- 
      map_dfr(
        .x = 1:n.hydro,
        .f = function(i) {
          error.ols <- rnorm(nrow(runoff), sd = summary(model)$sigma)
          error.extreme <- rnorm(nrow(runoff), sd = sd(extreme.resid))
          error.mixture <-
            cbind(error.ols, error.extreme) %>% as.data.frame %>%
            mutate(id = sample(c(0,1), size = nrow(runoff), replace = TRUE,
                               prob = c(mixratio, 1-mixratio))) %>%
            mutate(error.mix = case_when(id==0 ~ error.ols, id==1 ~ error.extreme)) %>%
            pull(error.mix)
          runoff %>% mutate(Qp_m3s = prediction + error.mixture*se.mult, n.hydro = i)
        })
  } else hydrograph <- runoff %>% rename(Qp_m3s = prediction) %>% mutate(n.hydro = NA)
  ## add baseflow
  hydrograph <- hydrograph %>% mutate(Qp_m3s = ifelse(Qp_m3s < 0, 0, Qp_m3s) + 4) 

  ## generate new realizations of tp
  fit.tp <- fitdist(catalog$tp_hrs[!is.na(catalog$tp_hrs)], 'lnorm')$estimate
  if (probabilistic) {
    hydrograph <- hydrograph %>% 
      mutate(tp_hrs = rlnorm(nrow(.), meanlog = fit.tp[1], sdlog = fit.tp[2]))
  } else hydrograph <- hydrograph %>% mutate(tp_hrs = exp(fit.tp[1]))
  
  ## return fitted model objects
  fit.tp <<- fit.tp
  model.Qp <<- model

  ## return hydrograph dataframe
  hydrograph <- hydrograph %>% 
    select(n.AR, n.precip, n.hc, n.runoff, n.hydro, IVT_max, duration, 
           precip_mm, runoff_mm, sm, Qp_m3s, tp_hrs)
  if (prcp.memory) hydrograph$n.precip <- rep(NA, nrow(hydrograph))
  if (hc.memory) hydrograph$n.hc <- rep(NA, nrow(hydrograph))
  if (rnff.memory) hydrograph$n.runoff <- rep(NA, nrow(hydrograph))
  return(hydrograph)
}
