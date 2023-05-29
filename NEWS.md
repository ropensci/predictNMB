# predictNMB (development version)

* add `wtp_linetype` argument to `ce_plot()` (and default to `"dashed"`) to 
  given control to user and differentiate C-E plane from main axes.

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
