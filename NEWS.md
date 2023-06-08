# predictNMB (development version)

# predictNMB 0.2.1

* `ce_plot()`

  * add `wtp_linetype` argument to `ce_plot()` (and default to `"dashed"`) to 
  given control to user and differentiate C-E plane from main axes.
  
  * add `shape` argument which maps to the shape aesthetic in `geom_point()`. 
  User can also specify `shape = "cost-effective"` to map it to whether that
  point is under the WTP/cost-effectiveness plane, or `shape = "method"` to map
  it to the cutpoint method.
  
  * add `add_prop_ce` argument. If `TRUE`, the % of points for that
  cutpoint method is concatenated to the the method name in the figure legend.
  
  * outcome costs were incorrectly excluded from the costs used in the 
  incremental cost axis before. These are now added to the calculations in 
  `evaluate_cutpoint_cost()` and reflected in the `ce_plot()`. Before, the
  incremental costs in `ce_plot()` were treatment costs alone.

# predictNMB 0.2.0

* Track QALYs and intervention costs
  
  * Track QALYs and intervention costs separately from NMB within simulations
  
  * `get_nmb_sampler()` now returns a `NMBsampler` object with attributes to 
    indicate whether QALYs should be tracked during simulations and the WTP used
  
  * If QALYs are tracked, `ce_plot()` can be used to create a cost-effectiveness
    plot

# predictNMB 0.1.0

* Updates to README, vignettes and documentation for clarity

* Many updates in response to peer review with ropensci see [#566 (response comment)](https://github.com/ropensci/software-review/issues/566#issuecomment-1489580791)

  * Efficiency improvements and generally improved coding style
  
  * Use of `autoplot()` method instead of `plot()`
  
  * Use of `summary()` method instead of `make_summary_table()`
  
  * Removal of default themes in plots in exchange for `... + theme_sim()`
  
  * Improvements and corrections to vignettes


# predictNMB 0.0.1

* First submitted version of predictNMB to rOpenSci

* Includes functions for simulating and evaluating clinical prediction models 
  regarding their the estimated Net Monetary Benefit (NMB)
