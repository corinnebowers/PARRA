
###################################################################################################

## Note: the fitting of the runoff model and the generation of new runoff realizations are combined
## in one function, because the bootstrapping process for CN parameters makes it difficult to 
## disentangle these two processes.

fit_generate_runoff <- function(
  precip, catalog, probabilistic = FALSE, n.runoff = 1, boot = 1e3) {
  #'
  #' Calculates the curve number (CN) for the area of interest based on the catalog of 
  #' precipitation and runoff records. Generates new realizations of runoff based on precipitation 
  #' and soil moisture.
  #' 
  #' @param precip Dataframe of precipitation events to generate runoff for. Columns "n.AR", 
  #' "n.precip", "n.hc", "IVT_max", "duration", "precip_mm", and "sm" must be present.
  #' @param catalog Catalog of ARs occurring in area of interest. Columns "IVT_max", "duration",
  #' "precip_mm", "runoff_mm", and "wy" must be present.
  #' @param probabilistic Logical binary that indicates whether to incorporate uncertainty into 
  #' new runoff realizations.
  #' @param n.runoff Number of runoff realizations to generate per precipitation event.
  #' 
  #' @return a dataframe of synthetic runoff realizations.
  #' 
  
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
      S = qnorm(p = (sm-min(sm)+0.5)/(max(sm)-min(sm)+1), mean = S.mean, sd = S.sd),
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
        TRUE ~ (precip_in-0.2*(S))^2/(precip_in+0.8*(S))),
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

fit_Qp <- function(catalog, extremeratio = 0.1) {
  #'
  #' Fits a ordinary least squares linear regression model to peak streamflow records in the 
  #' catalog for use in the PARRA framework. 
  #' 
  #' @param catalog Catalog of ARs occurring in area of interest. Columns "precip_mm", "runoff_mm",
  #' and "Qp_m3s" must be present.
  #' @param extremeratio Fraction of AR events in the catalog to be used in fitting the 
  #' extremes component of the residuals mixture model. For example, a value of 0.1 means that 
  #' the standard deviation of the extremes is calculated based on the top 10% of AR events.
  #'
  #' @return "model.Qp", the fitted lm regression model object; and "se.Qp", the fitted 
  #' parameters for the residuals mixture model.
  #' 
  
  ## fit linear regression to predict Qp
  model.Qp <- lm(Qp_m3s ~ precip_mm*runoff_mm, data = catalog)
  se.ols <- summary(model.Qp)$sigma
  
  ## save out regression object
  model.Qp <<- model.Qp
  
  ## characterize distribution of extreme residuals
  extreme.id <- catalog %>% 
    mutate(n.AR = 1:nrow(.)) %>% 
    filter(precip_mm >= quantile(precip_mm, 1-extremeratio) & 
             runoff_mm >= quantile(runoff_mm, 1-extremeratio)) %>% pull(n.AR)
  extreme.resid <- model.Qp$residuals[extreme.id]
  se.extreme <- sd(extreme.resid)

  ## save out error model
  se.Qp <<- c('se.ols' = se.ols, 'se.extreme' = se.extreme)
}


###################################################################################################

fit_tp <- function(catalog) {
  #'
  #' Fits a lognormal distribution to time to peak streamflow records in the catalog for 
  #' use in the PARRA framework. 
  #' 
  #' @param catalog Catalog of ARs occurring in area of interest. 
  #' Column "tp_hrs" must be included.
  #'
  #' @return "model.prcp", the fitted lm regression model object; and "se.prcp", the 
  #' fitted parameters for the residuals mixture model.
  #' 
  fit.tp <<- fitdist(catalog$tp_hrs[!is.na(catalog$tp_hrs)], 'lnorm')$estimate
}


###################################################################################################

generate_hydrograph <- function(
  runoff, model.Qp, se.Qp, mixratio = 0.9, fit.tp, probabilistic = FALSE, n.hydro = 1) {
  #'
  #' Generates new realizations of hydrograph parameters Qp (peak streamflow) and tp 
  #' (time to peak streamflow) based on precipitation and runoff.
  #' 
  #' @param runoff Dataframe of runoff events to generate hydrograph parameters for. Columns 
  #' "n.AR", "n.precip", "n.hc", "n.runoff", "precip_mm", "runoff_mm", and "sm" must be present.
  #' @param model.Qp Fitted lm regression model object for peak streamflow.
  #' @param se.prcp Two-element vector of fitted parameters for the peak streamflow residuals 
  #' mixture model.
  #' @param mixratio Fraction of points to be pulled from the robust standard error 
  #' distribution. The remainder of errors in the mixture model will be pulled from the 
  #' distribution of extremes.
  #' @param fit.tp Fitted lognormal parameters for time to peak streamflow based on the catalog.
  #' @param probabilistic Logical binary that indicates whether to incorporate uncertainty into 
  #' new hydrograph parameter realizations.
  #' @param n.hydro Number of hydrograph parameter realizations to generate per runoff event.
  #' 
  #' @return a dataframe of synthetic hydrograph parameter realizations.
  #' 
  
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

  ## generate new realizations of Qp and tp
  runoff$prediction <- predict(model.Qp, runoff)
  if (probabilistic) { #add stochastic uncertainty to realizations
    hydrograph <- 
      map_dfr(
        .x = 1:n.hydro,
        .f = function(i) {
          error.ols <- rnorm(nrow(runoff), sd = se.Qp[1])
          error.extreme <- rnorm(nrow(runoff), sd = se.Qp[2])
          error.mixture <-
            cbind(error.ols, error.extreme) %>% as.data.frame %>%
            mutate(id = sample(c(0,1), size = nrow(runoff), replace = TRUE,
                               prob = c(mixratio, 1-mixratio))) %>%
            mutate(error.mix = case_when(id==0 ~ error.ols, id==1 ~ error.extreme)) %>%
            pull(error.mix)
          runoff %>% 
            mutate(Qp_m3s = (prediction + error.mixture) %>% 
                     cbind(0) %>% apply(1, max)) %>% 
            mutate(n.hydro = i)
        }) %>% 
      mutate(tp_hrs = rlnorm(nrow(.), meanlog = fit.tp[1], sdlog = fit.tp[2]))
    
  } else { #report deterministic expected value
    hydrograph <- runoff %>% 
      mutate(Qp_m3s = ifelse(prediction < 0, 0, prediction),
             tp_hrs = exp(fit.tp[1])) %>% 
      select(-prediction) %>% 
      mutate(n.hydro = NA)
  }

  ## return hydrograph dataframe
  hydrograph <- hydrograph %>% 
    select(n.AR, n.precip, n.hc, n.runoff, n.hydro, IVT_max, duration, 
           precip_mm, runoff_mm, sm, Qp_m3s, tp_hrs)
  if (prcp.memory) hydrograph$n.precip <- rep(NA, nrow(hydrograph))
  if (hc.memory) hydrograph$n.hc <- rep(NA, nrow(hydrograph))
  if (rnff.memory) hydrograph$n.runoff <- rep(NA, nrow(hydrograph))
  return(hydrograph)
}


###################################################################################################

