test_that("screen_simulation_inputs() works", {
  get_nmb <- function() c("TP" = 1, "TN" = 2, "FP" = 3, "FN" = 4)
  sim_screen_obj <- screen_simulation_inputs(
    n_sims = 10, n_valid = 1000, sim_auc = seq(0.7, 0.9, 0.1), event_rate = 0.1,
    fx_nmb_training = get_nmb, fx_nmb_evaluation = get_nmb
  )
  expect_s3_class(sim_screen_obj, "predictNMBscreen")
})

test_that("plot method - defaults - works", {
  obj <- readRDS(test_path("fixtures", "predictNMBscreen_object.rds"))
  expect_s3_class(plot(obj), "gg")
})

test_that("plot method - selected x_axis_var - works", {
  obj <- readRDS(test_path("fixtures", "predictNMBscreen_object.rds"))
  p <- plot(obj, x_axis_var="event_rate")
  expect_s3_class(p, "gg")
})

test_that("plot method - selected constants - works", {
  obj <- readRDS(test_path("fixtures", "predictNMBscreen_object.rds"))
  p <- plot(obj, x_axis_var="event_rate", constants=list(sim_auc=0.8))
  expect_s3_class(p, "gg")
})
