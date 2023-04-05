
<!-- README.md is generated from README.Rmd. Please edit that file -->

# predictNMB <a href='https://docs.ropensci.org/predictNMB/'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/ropensci/predictNMB/workflows/R-CMD-check/badge.svg)](https://github.com/ropensci/predictNMB/actions)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Codecov test
coverage](https://codecov.io/gh/ropensci/predictNMB/branch/master/graph/badge.svg)](https://app.codecov.io/gh/ropensci/predictNMB?branch=master)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Status at rOpenSci Software Peer
Review](https://badges.ropensci.org/566_status.svg)](https://github.com/ropensci/software-review/issues/566)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.05328/status.svg)](https://doi.org/10.21105/joss.05328)
<!-- badges: end -->

## Overview

predictNMB is a tool to evaluate (hypothetical) clinical prediction
models based on their estimated Net Monetary Benefit (NMB). It may be
used by prediction model developers who intend to find out how
performant their model needs to be to be clinically useful or by those
in health services deciding whether or not to implement an existing
model.

`{predictNMB}` has two main functions:

- `do_nmb_sim()`: takes user defined inputs for a given prediction model
  and population, then evaluates the NMB by performing simulations.
- `screen_simulation_inputs()`: calls `do_nmb_sim()` many times, using a
  range of values for any of its inputs. This is useful for sensitivity
  analysis.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("ropensci/predictNMB")

# or, equivalently:
install.packages("predictNMB", repos = "https://ropensci.r-universe.dev")
```

## Estimating model cutpoints

We must first define a hypothetical NMB associated with each square of a
confusion matrix (2x2 table). To do this, we create an R function which,
when run, returns a named vector representing the four NMB values that
we need. `get_nmb_sampler()` is a `predictNMB` function that makes
creating the function easier!

``` r
library(predictNMB)

fx_nmb <- get_nmb_sampler(
  outcome_cost = 100,
  high_risk_group_treatment_effect = 0.35,
  high_risk_group_treatment_cost = 10
)

fx_nmb()
#>   TP   FP   TN   FN 
#>  -75  -10    0 -100
```

We can then pass this to the simulation function. Required arguments:

- `n_sims`: number of simulations to run. More simulations take longer,
  but are more stable.
- `event_rate`: event incidence rate, or the proportion of patients
  experiencing the event.
- `sim_auc`: vector of hypothetical AUCs; e.g. `seq(0.7, 0.95, 0.05)` or
  `c(0.75, 0.80, 0.85)`.
- `n_valid`: number of samples the validation set draws within each
  simulation (evaluating the NMB under each cutpoint).
- `fx_nmb_training`: function that returns a named vector used to get
  cutpoints on the training set. Recommended to use constant values.
- `fx_nmb_evaluation`: function that returns a named vector used to get
  cutpoints on the evaluation set. Recommended to use sampled values.
- `cl`: (Optional) users can pass a cluster made using
  `parallel::makeCluster()`. If it is given, the simulations are run in
  parallel (faster).

``` r
library(parallel)
cl <- makeCluster(detectCores())
```

``` r
sim_screen_obj <- screen_simulation_inputs(
  n_sims = 1000, n_valid = 10000, sim_auc = seq(0.7, 0.95, 0.05), event_rate = 0.1,
  fx_nmb_training = fx_nmb, fx_nmb_evaluation = fx_nmb,
  cutpoint_methods = c("all", "none", "youden"), cl = cl
)
```

These simulations can be interpreted as a range of hypothetical
situations under different levels of model performance within our
specific healthcare setting. We can visualise how this change may affect
preferences between the model-guided strategy versus a treat-all or
treat-none strategy — in other words, using the model to determine who
should get treatment, rather than everyone or no-one.

`autoplot()` can be used on the object returned from this function to
quickly inspect these trends:

``` r
autoplot(sim_screen_obj)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

Here, we are visualising many simulations under different inputs. If we
just want to inspect a single set of inputs, say when the model AUC is
0.9, we can run that simulation alone using `do_nmb_sim()`, or access it
from our existing screen.

``` r
single_sim_obj <- do_nmb_sim(
  n_sims = 1000, n_valid = 10000, sim_auc = 0.9, event_rate = 0.1,
  fx_nmb_training = fx_nmb, fx_nmb_evaluation = fx_nmb,
  cutpoint_methods = c("all", "none", "youden"), cl = cl
)
```

``` r
single_sim_obj <- sim_screen_obj$simulations[[6]]
```

When plotting a single set of simulation inputs, we see the
distributions of the NMB across all simulations under each strategy.
Note that we have added some tasteful themes using `theme_sim()` that
help reduce clutter on these types of plots.

``` r
autoplot(single_sim_obj) + theme_sim()
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

## Citation

``` r
citation("predictNMB")
#> 
#> To cite predictNMB in publications use:
#> 
#>   Parsons et al., (2023). predictNMB: An R package to estimate if or
#>   when a clinical prediction model is worthwhile. Journal of Open
#>   Source Software, 8(84), 5328, https://doi.org/10.21105/joss.05328
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Article{,
#>     title = {predictNMB: An R package to estimate if or when a clinical prediction model is worthwhile},
#>     author = {Rex Parsons and Robin D Blythe and Adrian G Barnett and Susanna M Cramb and Steven M McPhail},
#>     journal = {Journal of Open Source Software},
#>     publisher = {The Open Journal},
#>     year = {2023},
#>     volume = {8},
#>     number = {84},
#>     pages = {5328},
#>     url = {https://doi.org/10.21105/joss.05328},
#>     doi = {10.21105/joss.05328},
#>   }
```

## Further reading

The [`predictNMB` website](https://docs.ropensci.org/predictNMB/) and
its vignettes:

- [Getting started with
  `predictNMB`](https://docs.ropensci.org/predictNMB/articles/predictNMB.html)
- [Summarising results from
  `predictNMB`](https://docs.ropensci.org/predictNMB/articles/summarising-results-with-predictNMB.html)
- [Detailed example: pressure injury
  model](https://docs.ropensci.org/predictNMB/articles/detailed-example.html)
- [Creating NMB
  functions](https://docs.ropensci.org/predictNMB/articles/creating-nmb-functions.html)

## Contributing

Please note that this package is released with a [Contributor Code of
Conduct](https://ropensci.org/code-of-conduct/). By contributing to this
project, you agree to abide by its terms.

- If you think you have encountered a bug, please [submit an
  issue](https://github.com/ropensci/predictNMB).

- Please include a
  [reprex](https://reprex.tidyverse.org/articles/articles/learn-reprex.html)
  (a minimal, reproducible example) to clearly communicate about your
  code.
