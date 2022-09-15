
<!-- README.md is generated from README.Rmd. Please edit that file -->

# predictNMB <a href='https://github.com/RWParsons/predictNMB'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/RWParsons/predictNMB/workflows/R-CMD-check/badge.svg)](https://github.com/RWParsons/predictNMB/actions)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

## Overview

predictNMB is a tool to evaluate clinical prediction models based on
their estimated Net Monetary Benefit (NMB).

`{predictNMB}` has two main functions:

-   `do_nmb_sim()`: takes user defined inputs for a given prediction
    model and population, then evaluates the NMB by performing
    simulations.
-   `screen_simulation_inputs()`: calls `do_nmb_sim()` many times, using
    a range of values for any of its inputs. This is useful for
    sensitivity analysis.

## Installation

You can install the development version of predictNMB from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("RWParsons/predictNMB")
```

## Estimating model cutpoints

Hypothetical NMB associated with each square of a confusion matrix must
first be defined.

``` r
library(predictNMB)

fx_nmb <- function() {
  c(
    "TP" = -75, # True positive
    "FP" = -10, # False positive
    "TN" = 0,  # True negative
    "FN" = -100 # False negative
  )
}

fx_nmb()
#>   TP   FP   TN   FN 
#>  -75  -10    0 -100
```

Required arguments:

-   `n_sims`: number of simulations to run. More simulations take
    longer, but are more stable <br>
-   `event_rate`: event incidence rate, or the proportion of patients
    experiencing the event <br>
-   `sim_auc`: vector of hypothetical AUCs; e.g. `seq(0.7, 0.95, 0.05)`
    or `c(0.75, 0.80, 0.85)` <br>
-   `n_valid`: number of samples the validation set draws within each
    simulation (evaluating the NMB under each cutpoint) <br>
-   `fx_nmb_training`: function-defined vector used to get cutpoints on
    the training set. Recommended to use constant values <br>
-   `fx_nmb_evaluation`: function-defined vector used to get cutpoints
    on the evaluation set. Recommended to use sampled values <br>
-   (Optional) Users can pass a cluster as the `cl` argument. If it is
    passed, the simulations are run in parallel (faster). <br>

``` r
library(parallel)
cl <- makeCluster(detectCores())
sim_screen_obj <- screen_simulation_inputs(
  n_sims = 1000, n_valid = 10000, sim_auc = seq(0.7, 0.95, 0.05), event_rate = 0.1,
  fx_nmb_training = fx_nmb, fx_nmb_evaluation = fx_nmb,
  cutpoint_methods = c("all", "none", "youden", "value_optimising"), cl = cl
)
```

`plot()` can be used to generate plots of the many simulations.

``` r
plot(sim_screen_obj)
#> No value for 'x_axis_var' given.
#> Screening over sim_auc by default
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

The plot method includes additional arguments to rename the methods
using a named vector. It can also be used to visualise cutpoints or the
Incremental Net Monetary Benefit (INB) when there’s a known reference
strategy.

``` r
plot(
  sim_screen_obj, 
  rename_vector=c("Treat All" = "all", "Treat None" = "none", "Youden Index" = "youden", "Value-Optimising" = "value_optimising")
)
#> No value for 'x_axis_var' given.
#> Screening over sim_auc by default
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

``` r
plot(
  sim_screen_obj, 
  rename_vector=c("Treat All" = "all", "Treat None" = "none", "Youden Index" = "youden", "Value-Optimising" = "value_optimising"),
  what="inb",
  inb_ref_col="Treat All"
)
#> No value for 'x_axis_var' given.
#> Screening over sim_auc by default
```

<img src="man/figures/README-unnamed-chunk-5-2.png" width="100%" />

By default, the median (dot), 95% interval (thick vertical line), range
(skinny vertical line), and lines between points are shown. All except
the dots can be optionally turned off or on, and the width of the
interval can be controlled with `ci`.

``` r
plot(
  sim_screen_obj,
  plot_line = FALSE,
  plot_range = FALSE,
  plot_ci = 0.8
)
#> No value for 'x_axis_var' given.
#> Screening over sim_auc by default
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

``` r
plot(
  sim_screen_obj,
  plot_line = TRUE,
  plot_range = FALSE,
  plot_ci = 0
)
#> No value for 'x_axis_var' given.
#> Screening over sim_auc by default
```

<img src="man/figures/README-unnamed-chunk-6-2.png" width="100%" />

The simulations for a given set of inputs can be accessed from our
`predictNMBscreen` object (`sim_screen_obj`). These are the same as the
output from `do_nmb_sim()` and have their own methods as well.

``` r
sim_screen_obj$simulations
#> [[1]]
#> predictNMB object
#> 
#> Training data sample size:  189
#> Minimum number of events in training sample:  19
#> Evaluation data sample size:  10000
#> Number of simulations:  1000
#> Simulated AUC:  0.7
#> Simulated event rate:  0.1
#> [[2]]
#> predictNMB object
#> 
#> Training data sample size:  139
#> Minimum number of events in training sample:  14
#> Evaluation data sample size:  10000
#> Number of simulations:  1000
#> Simulated AUC:  0.75
#> Simulated event rate:  0.1
#> [[3]]
#> predictNMB object
#> 
#> Training data sample size:  139
#> Minimum number of events in training sample:  14
#> Evaluation data sample size:  10000
#> Number of simulations:  1000
#> Simulated AUC:  0.8
#> Simulated event rate:  0.1
#> [[4]]
#> predictNMB object
#> 
#> Training data sample size:  139
#> Minimum number of events in training sample:  14
#> Evaluation data sample size:  10000
#> Number of simulations:  1000
#> Simulated AUC:  0.85
#> Simulated event rate:  0.1
#> [[5]]
#> predictNMB object
#> 
#> Training data sample size:  139
#> Minimum number of events in training sample:  14
#> Evaluation data sample size:  10000
#> Number of simulations:  1000
#> Simulated AUC:  0.9
#> Simulated event rate:  0.1
#> [[6]]
#> predictNMB object
#> 
#> Training data sample size:  139
#> Minimum number of events in training sample:  14
#> Evaluation data sample size:  10000
#> Number of simulations:  1000
#> Simulated AUC:  0.95
#> Simulated event rate:  0.1
sim_obj <- sim_screen_obj$simulations[[1]]

sim_obj
#> predictNMB object
#> 
#> Training data sample size:  189
#> Minimum number of events in training sample:  19
#> Evaluation data sample size:  10000
#> Number of simulations:  1000
#> Simulated AUC:  0.7
#> Simulated event rate:  0.1
```

Here, using the `predictNMBsim` object, the plot shows the median as the
solid bar within the distribution and the light blue portion of the
distribution is the 95% interval (level can be changed using the `ci`
argument).

``` r
plot(sim_obj)
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

``` r
plot(sim_obj, ci=0.50)
```

<img src="man/figures/README-unnamed-chunk-8-2.png" width="100%" />

The plot methods for both the `predictNMBscreen` and `predictNMBsim`
objects have almost the same usage of arguments.

``` r
plot(
  sim_obj, 
  rename_vector=c("Treat All" = "all", "Treat None" = "none", "Youden Index" = "youden", "Value-Optimising" = "value_optimising")
)
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

``` r
plot(
  sim_obj, 
  rename_vector=c("Treat All" = "all", "Treat None" = "none", "Youden Index" = "youden", "Value-Optimising" = "value_optimising"),
  what="inb",
  inb_ref_col="Treat All"
)
```

<img src="man/figures/README-unnamed-chunk-9-2.png" width="100%" />

## Further reading

The vignette, Introduction to predictNMB, is available here (insert
link)

## Related work

This R package is based on previous work on value-optimising cutpoints
for the economic evaluation of clinical prediction models. It can be
accessed here (insert link).
