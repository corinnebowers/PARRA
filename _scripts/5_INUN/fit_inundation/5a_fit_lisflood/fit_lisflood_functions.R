
#### Determine accuracy metrics ###################################################################

confusion <- function(obs, sim) {
  #'
  #' Calculates the confusion matrix between two binary vectors 
  #'
  #' @param obs The "observed" data vector.
  #' @param sim The "simulated" data vector.
  #' 
  #' @return Reduced set of confusion matrix values, eliminating the true positive case.
  #' 
  ifelse(obs > 0 & sim > 0, 0, 
         ifelse(obs > 0 & sim <= 0, -1, 
                ifelse(obs <= 0 & sim > 0, 1, NA)))
}

binary <- function(x) {
  #'
  #' Collapses a raster of non-negative numbers into a matrix of binary numbers, where all values
  #' greater than 0 are set to 1. 
  #' 
  #' @param x The raster under consideration.
  #' 
  #' @return A matrix of ones and zeros.
  #' 
  mat <- as.matrix(x) 
  mat <- ifelse(is.na(mat[]), 0, ifelse(mat[]>0, 1, 0))
  return(mat)
}


#### Attempt model falsification ##################################################################


#### Examine parameter sensitivity ################################################################

L1_boot <- function(data, col, cl, var, boot = 1e3) {
  #' 
  #' Finds the L1 norm of the distance between two vectors. 
  #' 
  #' @param data Dataframe with parameter values & clustering information.
  #' @param col Name of clustering column.
  #' @param cl Cluster number to consider.
  #' @param var Parameter to consider.
  #' @param boot Number of bootstrapping samples to draw.
  #'
  #' @return A vector of bootstrapped L1 norm estimates.
  #' 
  x <- sort(data[data[,col] == cl, var])
  x.all <- sort(data[,var])
  p <- (1:length(x))/(length(x)+1)
  p.all <- (1:length(x.all))/(length(x.all)+1)
  x.interp <- interp1(x = p.all, y = x.all, xi = p)
  
  if (is.na(boot)) {
    return(cbind(x, x.interp) %>% apply(1, diff) %>% abs %>% sum)
  } else {
    L1 <- 
      foreach (b = 1:boot, .combine = 'c') %do% {
        sample(x.all, size = length(x), replace = TRUE) %>% sort %>% 
          cbind(x.interp) %>% apply(1, diff) %>% abs %>% sum
      }
    return(L1)
  }
  
  # g <- ggplot()
  # for (b in 1:100) {
  #   g <- g + geom_line(aes(x = sort(sample(x.all, size = length(x), replace = TRUE)), y = p), alpha = 0.1)
  # }
  # g +
  #   geom_step(data = data.frame(x.all, p.all),
  #             aes(x = x.all, y = p.all, color = 'x.all'), size = 1) +
  #   geom_step(data = data.frame(x, p, x.interp),
  #             aes(x = x, y = p, color = 'x'), size = 1) +
  #   geom_step(data = data.frame(x, p, x.interp),
  #             aes(x = x.interp, y = p, color = 'x.interp'), size = 1) +
  #   scale_x_origin() + scale_y_origin() +
  #   scale_color_manual(values = c('red', 'black', 'blue'))
  
  # ggplot() +
  #   geom_histogram(aes(x = L1), color = 'black', fill = 'white') +
  #   geom_vline(aes(xintercept = quantile(L1, 0.95), color = 'd95'),
  #              size = 1) +
  #   geom_vline(aes(xintercept = cbind(x, x.interp) %>%
  #                    apply(1, diff) %>% abs %>% sum, color = 'd'),
  #              size = 1) +
  #   scale_color_manual(values = c('black', 'red')) +
  #   scale_y_origin()
}

RSA <- function(k, plot = FALSE, data = FALSE) {
  #'
  #' Performs regional sensitivity analysis (RSA).
  #' 
  #' @param k Integer defining which cluster to evaluate.
  #' @param plot String naming which diagnostic plots to output.
  #' Can be TRUE, FALSE, or any combination of c('k', 'var', 'resa').
  #' @param data Logical determining whether to output the RSA hypothesis testing values.
  #' 
  #' @return Optional: diagnostic plots and/or dataframe of calculated hypothesis testing values.
  #' 
  
  ## perform k-means clustering on MDS distances
  kclust <- 
    mds.hd$points[-(n+1), 1:mds.hd$dim90] %>% 
    kmeans(centers = k, iter.max = 1000, nstart = 10)
  cluster.colors <- scico(palette = 'bamako', n = k+2)[2:(k+1)]
  
  ## attach MDS distances to parameter information
  df.cluster <- mds.hd$points[-(n+1), 1:2] %>% 
    as.data.frame %>% 
    setNames(c('x','y')) %>%
    mutate(id = cluster.id) %>% 
    left_join(samples.rp100, by = 'id') %>% 
    mutate(rsa.cluster = factor(kclust$cluster))
  
  ## generate 95% confidence intervals for every parameter & cluster
  cl <- parallel::makeCluster(num_cores)
  registerDoSNOW(cl)
  d <- 
    foreach(cl = 1:k, 
      .combine = 'rbind', 
      .export = 'L1_boot', 
      .packages = c('dplyr', 'pracma', 'foreach', 'scico'), 
      .inorder = FALSE) %:%
    foreach(var = vars, .combine = 'rbind', .inorder = FALSE) %dopar% {
      data.frame(
        cl = cl, var = var, 
        d = L1_boot(df.cluster, 'rsa.cluster', cl, var, NA),
        d95 = L1_boot(df.cluster, 'rsa.cluster', cl, var) %>% quantile(0.95) %>% unname)
    }
  stopCluster(cl)
  
  ## standardize L1-norm coefficients 
  d <- d %>% mutate(d.norm = d/d95, pass = d.norm>1)
  
  ## return results
  plot.RSA(plot, k, df.cluster, d, cluster.colors)
  if (data) df.cluster <<- df.cluster; return(d)
}

plot.RSA <- function(plot, k, df.cluster, d, cluster.colors) {
  #'
  #' Generates diagnostic plots for regional sensitivity analysis (RSA).
  #' 
  #' @param plot
  #' @param k
  #' @param df.cluster
  #' @param d
  #' @param cluster.colors
  #' 
  #' @return Optional: 
  #' Plot of k-means clustering in MDS space,
  #' Plot of clustered vs. full parameter distributions, and/or
  #' Plot of RSA results.
  #' 
  
  ## determine which plots to display
  plot.k <- ifelse(any(plot == TRUE) | 'k' %in% plot, TRUE, FALSE)
  plot.var <- ifelse(any(plot == TRUE) | 'var' %in% plot, TRUE, FALSE)
  plot.rsa <- ifelse(any(plot == TRUE) | 'rsa' %in% plot, TRUE, FALSE)
  
  ## plot k-means clustering in MDS space
  if (plot.k) {
    g <- ggplot(df.cluster) +
      geom_point(aes(x = x, y = y, color = factor(rsa.cluster)), size = 2) +
      labs(x = paste0('Dimension 1 (', percent(mds.hd$var[1], accuracy = 0.1), ')'),
           y = paste0('Dimension 2 (', percent(mds.hd$var[2], accuracy = 0.1), ')')) +
      scale_color_manual('Cluster', values = cluster.colors)
    print(g)
  }
  
  ## plot clustered vs. full parameter distributions
  if (plot.var) {
    for (var in vars) {
      lab <- vars.df[which(vars.df$vars == gsub('X', '', var)), 'description']
      g <- df.cluster %>%
        arrange(get(var)) %>%
        mutate(p = (1:n)/(n+1)) %>%
        group_by(rsa.cluster) %>%
        mutate(p.cluster = (1:length(rsa.cluster))/(length(rsa.cluster)+1)) %>%
        ggplot() +
        geom_step(aes(x = get(var), y = p), size = 1) +
        geom_step(aes(x = get(var), y = p.cluster,
                      group = factor(rsa.cluster), color = factor(rsa.cluster)),
                  size = 1, show.legend = FALSE) +
        scale_x_continuous(paste0(gsub('X', '', var), ': ', lab)) +
        scale_y_origin('Cumulative Probability') +
        scale_color_manual('Cluster', values = cluster.colors)
      print(g)
    }
  }
  
  ## plot regional sensitivity analysis results
  if (plot.rsa) {
    g <- ggplot(d %>% mutate(cl = factor(cl, levels = rev(levels(factor(cl)))))) + 
      geom_col(aes(x = var, y = d.norm, group = cl, fill = cl, alpha = factor(pass)), 
               position = 'dodge', width = 0.5) + 
      geom_hline(yintercept = 1, linetype = 'dashed') + 
      scale_fill_manual(name = 'Clusters', values = rev(cluster.colors), 
                        guide = guide_legend(reverse = TRUE)) + 
      scale_alpha_manual(values = c(0.4, 1)) + guides(alpha = 'none') + 
      scale_y_origin(breaks = seq(0, 2, 0.25)) + 
      ggtitle(paste0('Regional Sensitivity Analysis Results (k = ', k, ')')) + 
      labs(y = 'Normalized L1-norm Coefficient') + 
      coord_flip(ylim = c(0, 2)) + 
      theme(axis.title.y = element_blank())
    print(g)
  }
}

