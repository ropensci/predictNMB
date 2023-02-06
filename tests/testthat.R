#' @srrstats {G5.2, G5.2a, G5.2b, G5.3, G5.4, G5.4a, G5.5, G5.6, G5.6a, G5.6b,
#' G5.8, G5.8a, G5.8b, G5.8c, G5.8d, G5.9, G5.9b, G5.11}
#' Covered by package tests.
#' @srrstats {EA6.0, EA6.0a, EA6.0b, EA6.0c, EA6.0d}
#' All return values are tested. Classes and expectations for graphics are
#' tested in in test-plot.R.
#' Summary tables are assessed in terms of class, dimensions and and expected
#' outputs (indirectly assessing column names) in test-make_summary_table.R
#' predictNMB specific S3 objects are assessed within test-nmb_sim.R and
#' test-screen_simulation_inputs.R
#' @srrstats {EA6.1} Plots are assessed using vdiffr. See 'test-plot.R'.

library(testthat)
library(predictNMB)

test_check("predictNMB")
