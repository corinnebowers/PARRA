
###################################################################################################

fit_soilmoisture <- function(catalog) {
  #'
  #' Fits a lognormal distribution to soil moisture records in the catalog to characterize the 
  #' antecedent hydrologic conditions component model, $f(HC)$, for use in the PARRA framework. 
  #' 
  #' @param catalog Catalog of ARs occurring in area of interest. 
  #' Column "sm" must be present.
  #'
  #' @return "fit.sm", the fitted lognormal parameters for soil moisture based on the catalog.
  #' 
  fit.sm <<- fitdist(catalog$sm, 'lnorm')$estimate
}


###################################################################################################

generate_soilmoisture <- function(
  precip, fit.sm, probabilistic = FALSE, n.hc = 1) {
  #'
  #' Generates new realizations of soil moisture.
  #' 
  #' @param precip Dataframe of precipitation to generate soil moisture values for. 
  #' Columns "n.AR" and "n.precip" must be present. Column "sm" must be present if you want 
  #' to use observed data, otherwise the function will generate new values.
  #' @param fit.sm Fitted lognormal parameters for soil moisture based on the catalog.
  #' @param probabilistic Logical binary that indicates whether to incorporate uncertainty into 
  #' new soil moisture realizations.
  #' @param n.hc Number of soil moisture realizations to generate per precipitation event.
  #' 
  #' @return a dataframe of synthetic soil moisture realizations.
  #' 
  
  ## fix input parameters
  if (!probabilistic) n.hc <- 1
  
  ## calculate probabilistic counters
  n.AR <- max(precip$n.AR)
  if (any(is.na(precip$n.precip))) {
    prcp.memory <- TRUE
    precip$n.precip <- 1
  } else prcp.memory <- FALSE
  n.precip <- max(precip$n.precip)
  
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
    
  } else precip.hc <- precip %>% mutate(n.hc = NA)

  ## return precipitation dataframe with simulated soil moisture
  return(precip.hc)
}


###################################################################################################
