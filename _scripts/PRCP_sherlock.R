
###################################################################################################

## @param
## AR (data.frame): list of synthetic ARs
## catalog (data.frame): catalog of ARs occurring in region of interest
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
      model <- rq(fun, data = catalog %>% rename(precip = precip_obs), tau = t)
      prediction <- predict(model)
      prediction <- ifelse(prediction < 0, 0, prediction)
      c(tau = t, RMSE = RMSE(sort(prediction), sort(catalog$precip_obs)))
    }
  tau.best <- temp %>% as.data.frame %>% .[which.min(.$RMSE), 'tau']
  model <- rq(fun, data = catalog %>% rename(precip = precip_obs), tau = tau.best)
  AR <- AR %>%
    mutate(precip.mean = predict.rq(model, AR),
           precip.sd = predict.se(model, catalog %>% rename(precip = precip_obs), AR))
  
  # ## fit weighted OLS model
  # catalog <- catalog %>%
  #   arrange(precip) %>%
  #   mutate(p = (1:nrow(.))/(nrow(.)+1)) %>%
  #   mutate(weights = case_when(p > 0.9 ~ 2, p > 0.5 ~ 1, TRUE ~ 0))
  # model.wgt <- lm(precip ~ IVT_max*duration, data = catalog, weights = weights)
  # model.ols <- lm(precip ~ IVT_max*duration, data = catalog)
  # AR <- AR %>%
  #   mutate(wgt = predict(model.wgt, .), se.wgt = predict.se(model.wgt, catalog, .),
  #          ols = predict(model.ols, .), se.ols = predict.se(model.ols, catalog, .)) %>%
  #   arrange(ols) %>%
  #   mutate(p = (1:nrow(.)/(nrow(.)+1)),
  #          offset = case_when(p > 0.5 ~ 0, TRUE ~ (1-(2*p))),
  #          precip.mean = wgt - (offset**wgt[1]),
  #          precip.sd = se.wgt*(1-offset) + se.ols*offset)
  
  ## generate precipitation realizations
  if (probabilistic) {
    precip <- 
      map_dfr(.x = 1:n.AR, 
        .f = function(i) {
          rnorm(n = n.precip, mean = AR$precip.mean[i], sd = AR$precip.sd[i]) %>% 
            data.frame(n.AR = i, n.precip = 1:n.precip, precip_sim = .)
        })
  } else {
    precip <- AR %>% transmute(n.AR, n.precip = NA, precip_sim = precip.mean)
  }
  
  ## return fitted model object
  model.prcp <<- model
  
  ## return precipitation dataframe
  precip <- precip %>% 
    mutate(precip_sim = case_when(precip_sim < 0 ~ 0, TRUE ~ precip_sim)) %>% 
    full_join(AR %>% select(-precip.mean, -precip.sd), ., by = 'n.AR')
  return(precip)
}
