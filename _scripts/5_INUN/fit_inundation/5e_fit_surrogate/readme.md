# e) Fit LISFLOOD surrogate model

The last step to fitting the inundation component model is to fit a
computationally efficient surrogate to predict the LISFLOOD inundation
map based only on the inputs *Q*<sub>*p*</sub> (peak flow, m^3/s) and
*t*<sub>*p*</sub> (time to peak flow, hrs).

    ## setup information
    source('_data/setup.R')
    source('_data/plots.R')

    ## set parallel backend
    num_cores <- 5

    ## load location information
    load('_data/lisflood/dem.Rdata')

    ## load catalog
    load('_data/catalog/catalog.Rdata')

# 1 Set up best-fit parameter optimization problem

## 1.1 Define the surrogate model

We first define the surrogate model.
<!-- We have chosen to use the inverse distance weighted (IDW) spatial interpolation method.  -->
We used the inverse distance weighting (IDW) spatial interpolation
method described in the equations below to generate inundation maps
within the Monte Carlo process and reduce the computational demand of
the PARRA framework. The IDW method has three hyperparameters to tune,
which are as follows:

-   *n* defines the size of the search neighborhood;
-   *p* is the power function coefficient, which defines the decay rate
    of the distance weighting; and
-   *α* defines the anisotropy (asymmetry of information content)
    between *Q*<sub>*p*</sub> and *t*<sub>*p*</sub>.

*M*<sup>\*</sup> = *Σ*<sub>*i* = 1</sub><sup>*n*</sup>*λ*<sub>*i*</sub>*M*<sub>*i*</sub>/*Σ*<sub>*i* = 1</sub><sup>*n*</sup>*λ*<sub>*i*</sub>

*λ*<sub>*i*</sub> = ∥**x**<sup>\*</sup>, **x**<sub>*i*</sub>∥<sup> − *p*</sup>

**x** = {*α* \* *z*<sub>*Q*<sub>*p*</sub></sub>, (1−*α*) \* *z*<sub>*t*<sub>*p*</sub></sub>}

*M*<sup>\*</sup> represents the unknown (target) inundation map we are
trying to predict. We calculate this map as the weighted sum of the *n*
closest maps. A map *M*<sub>*i*</sub> is defined as \`\`close’’ if its
hydrograph parameters
{*Q*<sub>*p*</sub>,*t*<sub>*p*</sub>}<sub>*i*</sub> are similar to the
target hydrograph parameters
{*Q*<sub>*p*</sub>,*t*<sub>*p*</sub>}<sup>\*</sup>.  
*Q*<sub>*p*</sub><sup>\*</sup> and *t*<sub>*p*</sub><sup>\*</sup> are
known values used as inputs to the IDW interpolator function. However,
they are not Euclidean coordinates, so **x**<sup>\*</sup> is the
coordinate vector that represents
{*Q*<sub>*p*</sub>,*t*<sub>*p*</sub>}<sup>\*</sup> in modified parameter
space.

<!-- %after applying a normal score transformation and an anisotropy correction factor $\alpha$.  -->

We calculate the L2 (Euclidean) distance between **x**<sup>\*</sup>, the
coordinate vector of the target map, and **x**<sub>*i*</sub>, the
coordinate vector of the *i*<sup>*t**h*</sup> closest map. The inverse
of this distance multiplied by the power function *p* is the calculated
weight *λ*<sub>*i*</sub> for map *M*<sub>*i*</sub>, as shown in the
second equation.

## 1.2 Define IDW hyperparameter values

The three hyperparameters that require calibration are *n*, *p*, and
*α*. We find the best-fit hyperparameters by performing 10-fold
cross-validation on Sherlock, Stanford’s high-performance computing
cluster. An outline of that process is enumerated below.

1.  Define the values to consider for each hyperparameter in the
    user-defined section of `fit_surrogate.R`.
2.  Run `fit_surrogate.sbatch` to calculate error for each sample index
    and hyperparameter value.

The values we considered for each hyperparameter are listed below.

    ## n = 1, 2, 3, 4, 5, 6, 8, 10, 12, 15, 20

    ## p = 0, 0.25, 0.5, 0.75, 1, 1.5, 2, 2.5, 3, 4, 5

    ## alpha = 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95

## 1.3 Define error metric

Within each fold of the 10-fold cross-validation, 10% of the data is
withheld for testing. The surrogate model predicts the unseen data, and
the error metric is some measurement of the difference between the
observed vs. predicted inundation map.

A cell-by-cell comparison of inundation predictions at every 40 m
*t**i**m**e**s* 40 m cell in the study area is computationally
intractable. Therefore we calculate aggregate error across the study
area domain in two ways. The RMSE (root mean squared error) is a common
error metric used in machine learning and parameter tuning. It is
attractive because it includes all data points while also penalizing
larger error values. The MAE (mean absolute error) is a less common but
more intuitive metric that weights all error values equally and measures
how far your predictions were off, on average, from the true value. We
then repeat this calculation two times: once on the cells that
experience inundation at least 1% of the time (10 of the pre-computed
LISFLOOD maps) and once on the cells that experience inundation at least
50% of the time. This is to focus in on the locations of the study area
that contribute the most to severe flood damage and flood impacts. The
extent of the study area inundation 1% of the time vs. 50% of the time
is shown in the figure below.

    ## plot 1% inundation area vs. 50% inundation area

Overall this gives us four error metrics to consider. However, these
metrics are just to move from a full map to a single summary number. The
results of the Sherlock computing give us 1,000 values for each
combination of *n*, *p*, and *α*, one for every pre-computed map. This
is shown in the histogram below for a single hyperparameter combination.
Therefore we aggregate our error metrics even further.

    # ## load error data
    # files <- paste0('_scripts/5_INUN/fit_inundation/5e_fit_surrogate/results/error', 1:10, '.csv')
    # columns <- cols(
    #   X1 = col_double(),
    #   sim = col_double(),
    #   n = col_double(),
    #   p = col_double(),
    #   alpha = col_double(),
    #   SRSS = col_double(),
    #   RMSE = col_double(),
    #   max.resid = col_double(),
    #   max.loc = col_double()
    # )
    # error <- foreach(file = files) %do% {
    #   read_csv(file, col_types = columns) %>% select(sim, n, p, alpha, RMSE, SRSS)} %>% 
    #   do.call(rbind, .)

    ## plot error histogram for one hyperparameter combination

# 2 Calculate best-fit IDW parameter values

Rather than the rigorous parameter fit process we conducted for the
LISFLOOD environmental parameters in **5a\_fit\_lisflood**, this is more
of a qualitative assessment. Using the interactive plotly tool in R, we
iteratively removed values of *n*, *p*, and *α* that led to unfavorable
error outcomes. A few iterations are shown below for demonstration
purposes.

    ## include some plots here 

    ## n = 1, 2, 3, 4, 5, 6, 8, 10, 12, 15, 20

    ## p = 0, 0.25, 0.5, 0.75, 1, 1.5, 2, 2.5, 3, 4, 5

    ## alpha = 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95

Some of the conclusions we were able to draw:

-   list here

The final best-fit values chosen for the surrogate model hyperparameters
are shown in the table below. These lead to an overall model error of
\_\_\_.

    ## report values as a table
