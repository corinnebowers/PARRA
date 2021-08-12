
###################################################################################################

## @param
## catalog (data.frame): catalog of ARs occurring in region of interest
## n.AR (integer): number of ARs to generate
## intensity.threshold (double): keep only AR events above this percentile (recommended 0.9)
##  (assumptions: generate 10x more points than needed, then pick the highest ones)

## @return
## AR (data.frame): list of synthetic ARs

generate_AR <- function(catalog, n.AR = 1, intensity.threshold = 0.9) {
  ## create the Gaussian copula
  create_AR_copula(catalog)

  ## generate new points from the copula
  z <- rmvnorm(n = round(n.AR/(1-intensity.threshold)), mean = c(0,0), sigma = RHO) %>% as.data.frame
  dz <- 1/(1-intensity.threshold)
  temp <- data.frame(id = 1:n.AR, base = floor(dz), add = 0)
  temp$add[sample(1:n.AR, size = nrow(z)-floor(dz)*n.AR, replace = FALSE)] <- 1

  z <- z %>%
    mutate(id = temp %>% apply(1, function(x) rep(x[1], x[2]+x[3])) %>% unlist %>% c,
           rank1 = rank(V1), rank2 = rank(V2)) %>% 
    mutate(rank = cbind(rank1, rank2) %>% apply(1, mean)) %>% 
    group_by(id) %>%
    summarize(index = which.max(rank), v1 = V1[index], v2 = V2[index]) %>%
    select(v1, v2) %>% as.matrix
  u <- pnorm(z)
  AR <- data.frame(
    n.AR = 1:n.AR,
    duration = qlnorm(u[,1], meanlog = param_duration['meanlog'], sdlog = param_duration['sdlog']),
    IVT_max = qgumbel(u[,2], loc = param_IVT['alpha'], scale = param_IVT['scale']))
  return(AR)
}


###################################################################################################

## @param
## catalog (data.frame): catalog of ARs occurring in region of interest

## @return
## RHO (matrix): 2x2 correlation matrix
## param_duration (vector): lognormal parameters for duration
## param_IVT (vector): Type I Gumbel parameters for max IVT

## note: this will throw a warning if the chosen distributions are not a good fit, as measured by the K-S test

create_AR_copula <- function(catalog) {
  ## find spearman rank coefficient
  rho_s <- cor.test(catalog$IVT_max, catalog$duration, method = 'spearman')$estimate

  ## convert spearman rank coefficient to linear correlation
  rho <- 2*sin(pi*rho_s/6)
  RHO <<- matrix(c(1, rho, rho, 1), nrow = 2, ncol = 2)

  ## find lognormal parameters for duration
  sdlog <- sqrt(log((sd(catalog$duration)/mean(catalog$duration))^2 + 1))
  meanlog <- log(mean(catalog$duration)) - sdlog^2/2
  param_duration <<- c('meanlog' = meanlog, 'sdlog' = sdlog)

  ## find Gumbel parameters for IVT
  rate <- pi/(sd(catalog$IVT_max)*sqrt(6))
  alpha <- mean(catalog$IVT_max) - 0.5772/rate
  param_IVT <<- c('alpha' = alpha, 'scale' = 1/rate)

  ## do the fits pass the K-S test?
  fit <- c()
  d_crit <- 1.36/sqrt(nrow(catalog))

  x <- sort(catalog$duration + rnorm(nrow(catalog), sd = 2))
  cdf_x <- (1:length(x))/(length(x)+1)
  cdf_lognormal <- plnorm(x, meanlog = meanlog, sdlog = sdlog)
  fit['duration'] <- max(abs(cdf_lognormal - cdf_x)) / d_crit

  x <- sort(catalog$IVT_max)
  cdf_x <- (1:length(x))/(length(x)+1)
  cdf_gumbel <- pgumbel(x, loc = alpha, scale = 1/rate)
  fit['IVT'] <- max(abs(cdf_gumbel - cdf_x)) / d_crit

  if (fit['duration'] > 1) {
    warning('Logarithmic distribution is not a good fit to duration in this study region')}
  if (fit['IVT'] > 1) {
    warning('Gumbel extreme value distribution is not a good fit to maximum IVT in this study region')
  }
}


###################################################################################################

## @param
## catalog (data.frame): catalog of ARs occurring in region of interest
## add.historic (data.frame): option to add historic ARs to exceedance plot, colored black
## add.synthetic (data.frame): option to add synthetic ARs to exceedance plot, colored red

## @return
## print plot to console

plot_AR_copula <- function(catalog, add.historic = NA, add.synthetic = NA) {
  ## creat exceedance plot
  IVT_seq <- seq(0, max(catalog$IVT_max)+50, 10)
  dur_seq <- seq(0, max(catalog$duration)+5, 1)
  exceed <- matrix(nrow = length(IVT_seq), ncol = length(dur_seq))
  for (i in 1:length(IVT_seq)) {
    for (j in 1:length(dur_seq)) {
      exceed[i,j] <- catalog[catalog$IVT_max >= IVT_seq[i] & catalog$duration >= dur_seq[j],] %>% nrow
    }
  }
  exceed <- data.frame(exceed)
  names(exceed) <- dur_seq
  exceed <- cbind(IVT_max = IVT_seq, exceed)
  exceed <- melt(exceed, id.vars = 'IVT_max', variable.name = 'duration', value.name = 'freq')
  exceed$duration <- toNumber(exceed$duration)

  g <- ggplot() +
    geom_raster(data = exceed, aes(x = IVT_max, y = duration, fill = freq/40)) +
    ggtitle('Storm Exceedance Totals') +
    labs(x = 'Max Storm IVT (kg/m/s)', y = 'Storm Duration (hrs)', fill = 'Storms/year') +
    lims(x = c(250, NA)) +
    coord_fixed(ratio = 8) +
    scale_fill_viridis_c()

  if (!is.na(add.historic)) {
    g <- g + 
      geom_point(data = add.historic,
                 aes(x = IVT_max, y = duration), shape = 21, fill = 'green', color = 'white')
  }
  if(!is.na(add.synthetic)) {
    g <- g + 
      geom_point(data = add.synthetic,
                 aes(x = IVT_max, y = duration), shape = 21, fill = 'red', color = 'white')
  }
  print(g)
}
