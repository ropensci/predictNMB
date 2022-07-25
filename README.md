
<!-- README.md is generated from README.Rmd. Please edit that file -->

# predictNMB <a href='https://github.com/RWParsons/predictNMB'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/RWParsons/predictNMB/workflows/R-CMD-check/badge.svg)](https://github.com/RWParsons/predictNMB/actions)
<!-- badges: end -->

The goal of predictNMB is to evaluate a hypothetical clinical prediction
model regarding it’s Net Monetary Benefit (NMB)

## Installation

You can install the development version of predictNMB from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("RWParsons/predictNMB")
```

## Example

``` r
library(predictNMB)
```

`{predictNMB}` has two main functions:

-   `do_nmb_sim()`: using settings from the user for the prediction
    model and “patient” population, evaluates the NMB by performing many
    simulations.
-   `screen_simulation_inputs()`: calls `do_nmb_sim()` many times using
    a range values for any of its inputs.

The most time consuming part of evaluating the NMB for a given setting
and model will be obtaining realistic estimates for the NMB associated
with the outcomes of the prediction model.

Here, I pretend that the cost associated with some healthcare event is
$100, when the treatment (that costs $10) is given, this reduces the
probability that the $100 event occurs by 35% (reducing cost to $65) but
has the additional cost of the treatment (bringing the total expected
cost to $75), and patients that were correctly predicted to not have the
event have zero cost.

``` r
fx_nmb <- function() {
  c(
    "TP" = -75, # cost of the outcome + cost of treatment - saving associated with its effectiveness
    "FP" = -10, # cost of treatment
    "TN" = 0,  # no cost of treatment or outcome
    "FN" = -100 # full cost of the outcome
  )
}

fx_nmb()
#>   TP   FP   TN   FN 
#>  -75  -10    0 -100
```

We use this as an inputs so that the NMB can be evaluated for each
simulation.

In the simulation below, I want to see how performant I need my model to
be to out perform a treat-all or treat-none strategy (i.e. how good does
a model need to be to be better than no model at all). I’m going to
select cutpoints using the Youden index as well as the cost_effective
cutpoint.

The event that I’m interested in happens to 10% of my patient population
so I use `event_rate = 0.1`. I’m going to provide a vector of possible
inputs for the model AUC so I use `sim_auc = seq(0.7, 0.95, 0.05)`

``` r
sim_screen_obj <- screen_simulation_inputs(
  n_sims = 500, n_valid = 10000, sim_auc = seq(0.7, 0.95, 0.05), event_rate = 0.1,
  fx_nmb_training = fx_nmb, fx_nmb_evaluation = fx_nmb,
  cutpoint_methods = c("all", "none", "youden", "cost_effective")
)
```

`plot()` can be used to generate plots of the many simulations.

``` r
plot(sim_screen_obj)
#> No value for 'x_axis_var' given.
#> Screening over sim_auc by default
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

In this example, the cost of the treatment is relatively cheap ($10) to
the effectiveness (reduces risk of outcome by 35%) of it and the cost of
the outcome ($100). The Youden index, which is a pretty commonly used
method to obtain a cutpoint, out performs treat-all when the model has
an AUC of about 0.85 or higher. Whereas, the cost-effective cutpoint
starts out similar to treat-all even at low levels of model AUC, but
improves as the AUC increases, staying ahead of the Youden index.

The plot method includes additional arguments to rename the methods
using a named vector. It can also be used to visualise cutpoints or the
Incremental Net Monetary Benefit (INB) when there’s a known reference
strategy.

``` r
plot(
  sim_screen_obj, 
  rename_vector=c("Treat All" = "all", "Treat None" = "none", "Youden Index" = "youden", "Cost-effective" = "cost_effective")
)
#> No value for 'x_axis_var' given.
#> Screening over sim_auc by default
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

``` r
plot(
  sim_screen_obj, 
  rename_vector=c("Treat All" = "all", "Treat None" = "none", "Youden Index" = "youden", "Cost-effective" = "cost_effective"),
  what="inb",
  inb_ref_col="Treat All"
)
#> No value for 'x_axis_var' given.
#> Screening over sim_auc by default
```

<img src="man/figures/README-unnamed-chunk-6-2.png" width="100%" />
