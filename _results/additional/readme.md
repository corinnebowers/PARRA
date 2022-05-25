# Precipitation

These plots show the observed vs. simulated basin-wide precipitation
totals for all AR events in the five wet seasons from WY2015-2019
(October 2014 – March 2019).

    #### set up precipitation plots ####

    ## load precipitation component model functions
    source('_scripts/2_PRCP/PRCP.R')

    ## fit precipitation regression
    fit_precip(catalog)

    ## define base plot
    g <- ggplot() + 
      scale_x_continuous(
        'Storm Total Precipitation (mm)',
        limits = c(0,500), expand = expansion(mult = c(0,0))) + 
      scale_y_continuous(
        'Prob of Occurrence', expand = expansion(mult = c(0,0.05))) + 
      facet_wrap(~date, ncol = 3) + 
      annotation_custom(grob = linesGrob(x = rep(1/5,2), y = unit(c(-2.75,0), 'pt'))) +
      annotation_custom(grob = linesGrob(x = rep(2/5,2), y = unit(c(-2.75,0), 'pt'))) +
      annotation_custom(grob = linesGrob(x = rep(3/5,2), y = unit(c(-2.75,0), 'pt'))) +
      annotation_custom(grob = linesGrob(x = rep(4/5,2), y = unit(c(-2.75,0), 'pt'))) +
      annotation_custom(grob = linesGrob(x = rep(5/5,2), y = unit(c(-2.75,0), 'pt'))) +
      annotation_custom(grob = linesGrob(x = c(0.5,1), y = c(0,0))) +
      coord_cartesian(clip = 'off') + 
      theme(strip.background = element_rect(fill = 'grey95', color = 'grey95'), 
            plot.margin = margin(5,10,5,5),
            panel.spacing.x = unit(0.25, 'in'),
            panel.spacing.y = unit(0.25, 'in'),
            panel.background = element_rect(fill = NA, color = 'grey95'),
            axis.text.x = element_text(angle = 60, hjust = 1))

    #### cat 1 events ####

    cat1 <- catalog %>%
      filter(wy %in% 2015:2019 & cat == 1) %>%
      arrange(start_day) %>%
      transmute(
        n.AR = 1:nrow(.), precip_obs = precip_mm, date = start_day,
        IVT_max, duration, sm)
    precip1 <-
      generate_precip(
        AR = cat1 %>% select(-precip_obs),
        model.prcp, se.prcp, probabilistic = TRUE, n.precip = 1e3)
    g + 
      geom_histogram(
        data = precip1,
        aes(x = precip_mm, y = ..density..), bins = sqrt(1e3), boundary = 0,
        color = 'black', fill = roma.colors[5], alpha = 0.6, size = 0.25) +
      ggtitle('Cat 1 ARs, Sonoma County, WY 2015-2019', 
              subtitle = 'Observed vs. Simulated Precipitation') +
      geom_vline(
        data = cat1, aes(xintercept = precip_obs), linetype = 'dashed', size = 1)

<img src="readme_files/figure-markdown_strict/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

    #### cat 2 events ####

    cat2 <- catalog %>%
      filter(wy %in% 2015:2019 & cat == 2) %>% 
      arrange(start_day) %>% 
      transmute(
        n.AR = 1:nrow(.), precip_obs = precip_mm, date = start_day, 
        IVT_max, duration, sm)
    precip2 <- 
      generate_precip(
        AR = cat2 %>% select(-precip_obs), 
        model.prcp, se.prcp, probabilistic = TRUE, n.precip = 1e3)
    g + 
      geom_histogram(
        data = precip2,
        aes(x = precip_mm, y = ..density..), bins = sqrt(1e3), boundary = 0,
        color = 'black', fill = roma.colors[4], alpha = 0.6, size = 0.25) +
      ggtitle('Cat 2 ARs, Sonoma County, WY 2015-2019', 
              subtitle = 'Observed vs. Simulated Precipitation') +
      geom_vline(
        data = cat2, aes(xintercept = precip_obs), linetype = 'dashed', size = 1)

<img src="readme_files/figure-markdown_strict/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

    #### cat 3 events ####

    cat3 <- catalog %>%
      filter(wy %in% 2015:2019 & cat == 3) %>% 
      arrange(start_day) %>% 
      transmute(
        n.AR = 1:nrow(.), precip_obs = precip_mm, date = start_day, 
        IVT_max, duration, sm)
    precip3 <- 
      generate_precip(
        AR = cat3 %>% select(-precip_obs), 
        model.prcp, se.prcp, probabilistic = TRUE, n.precip = 1e3)
    g + 
      geom_histogram(
        data = precip3,
        aes(x = precip_mm, y = ..density..), bins = sqrt(1e3), boundary = 0,
        color = 'black', fill = roma.colors[3], alpha = 0.6, size = 0.25) +
      ggtitle('Cat 3 ARs, Sonoma County, WY 2015-2019', 
              subtitle = 'Observed vs. Simulated Precipitation') +
      geom_vline(
        data = cat3, aes(xintercept = precip_obs), linetype = 'dashed', size = 1)

<img src="readme_files/figure-markdown_strict/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

    #### cat 4 events ####

    cat4 <- catalog %>%
      filter(wy %in% 2015:2019 & cat == 4) %>% 
      arrange(start_day) %>% 
      transmute(
        n.AR = 1:nrow(.), precip_obs = precip_mm, date = start_day, 
        IVT_max, duration, sm)
    precip4 <- 
      generate_precip(
        AR = cat4 %>% select(-precip_obs), 
        model.prcp, se.prcp, probabilistic = TRUE, n.precip = 1e3)
    g + 
      geom_histogram(
        data = precip4,
        aes(x = precip_mm, y = ..density..), bins = sqrt(1e3), boundary = 0,
        color = 'black', fill = roma.colors[2], alpha = 0.6, size = 0.25) +
      ggtitle('Cat 4 ARs, Sonoma County, WY 2015-2019', 
              subtitle = 'Observed vs. Simulated Precipitation') +
      geom_vline(
        data = cat4, aes(xintercept = precip_obs), linetype = 'dashed', size = 1)

<img src="readme_files/figure-markdown_strict/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

    #### cat 5 events ####

    cat5 <- catalog %>%
      filter(wy %in% 2015:2019 & cat == 5) %>% 
      arrange(start_day) %>% 
      transmute(
        n.AR = 1:nrow(.), precip_obs = precip_mm, date = start_day, 
        IVT_max, duration, sm)
    precip5 <-  
      generate_precip(
        AR = cat5 %>% select(-precip_obs), 
        model.prcp, se.prcp, probabilistic = TRUE, n.precip = 1e3)
    g + 
      geom_histogram(
        data = precip5,
        aes(x = precip_mm, y = ..density..), bins = sqrt(1e3), boundary = 0,
        color = 'black', fill = roma.colors[1], alpha = 0.6, size = 0.25) +
      ggtitle('Cat 5 ARs, Sonoma County, WY 2015-2019', 
              subtitle = 'Observed vs. Simulated Precipitation') +
      geom_vline(
        data = cat5, aes(xintercept = precip_obs), linetype = 'dashed', size = 1)

<img src="readme_files/figure-markdown_strict/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

# Loss

Here we present the observed vs. simulated loss for the six events cited
in the 2017 version of Sonoma County’s Hazard Mitigation Plan. The
following is an excerpt from that document showing notable storms in the
decades preceding the report.

<center>

<img src="D:/1-PARRA/_results/additional/readme_files/sonomahazardmitigationplan.png" id="id" class="class" style="width:70.0%;height:70.0%" />

</center>

We scaled the observed loss totals to 2019 dollars and compared them to
the simulated loss histograms for each event.

    #### losses for noteworthy storm events ####

    ## define observed loss
    loss.obs <- 
      data.frame(
        event = c('1995-01-06', '1995-03-08', '1996-12-26', 
                  '1998-02-02', '2005-12-30', '2014-12-10'),
        loss = 1e6 * c(21, 13.3, 31, 28, 104, 1.1)) %>% 
      mutate(year = year(event)) %>% 
      left_join(inflation, by = 'year') %>% 
      transmute(event, loss = loss*rate)

    ## load simulated loss
    loss.sim <- 
      foreach (event = loss.obs$event, .combine = 'rbind') %do% {
        load(paste0('_results/additional/', event, '/checkpoints/DV.Rdata'))
        data.frame(event = event, loss = loss.sim$loss)
      }

    ## plot observed vs. simulated
    ggplot() + 
      geom_histogram(
        data = loss.sim,
        aes(x = loss, y = ..density..), color = 'black', fill = 'grey90',
        binwidth = 5e6, boundary = 0, size = 0.25) + 
      facet_wrap(~event, ncol = 2, scales = 'free_y') + 
      annotation_custom(grob = linesGrob(x = rep(1/5,2), y = unit(c(-2.75,0), 'pt'))) +
      annotation_custom(grob = linesGrob(x = rep(2/5,2), y = unit(c(-2.75,0), 'pt'))) +
      annotation_custom(grob = linesGrob(x = rep(3/5,2), y = unit(c(-2.75,0), 'pt'))) +
      annotation_custom(grob = linesGrob(x = rep(4/5,2), y = unit(c(-2.75,0), 'pt'))) +
      annotation_custom(grob = linesGrob(x = rep(5/5,2), y = unit(c(-2.75,0), 'pt'))) +
      geom_vline(
        data = loss.obs,
        aes(xintercept = loss), linetype = 'dashed', size = 1) +
      scale_x_continuous(
        'Loss Estimate', 
        limits = c(0,250e6), expand = expansion(mult = c(0,0)), 
        labels = comma_format(scale = 1e-6, accuracy = 1, prefix = '$', suffix = 'M')) + 
      scale_y_continuous(
        'Prob of Occurrence', 
        expand = expansion(mult = c(0, 0.2))) + 
      coord_cartesian(clip = 'off') + 
      theme(strip.background = element_rect(fill = 'grey95', color = 'grey95'), 
            plot.margin = margin(5,10,5,5),
            panel.spacing.x = unit(0.25, 'in'),
            panel.spacing.y = unit(0.25, 'in'),
            panel.background = element_rect(fill = NA, color = 'grey95'),
            axis.text.x = element_text(angle = 60, hjust = 1))

<img src="readme_files/figure-markdown_strict/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />
