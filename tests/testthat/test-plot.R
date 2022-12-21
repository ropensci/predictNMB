### plot.predictNMBsim() tests

test_that("predictNMBsim - plot method (default) works", {
  obj <- readRDS(test_path("fixtures", "predictNMBsim_object.rds"))

  # test default plotting of predictNMBsim object
  predictNMBsim_plot <- plot(obj)
  expect_s3_class(predictNMBsim_plot, "gg")
  vdiffr::expect_doppelganger(
    "plot.predictNMBsim(obj) default",
    predictNMBsim_plot
  )
})

test_that("predictNMBsim - plot method (what) works", {
  obj <- readRDS(test_path("fixtures", "predictNMBsim_object.rds"))

  # test cutpoints plot from predictNMBsim object
  predictNMBsim_cutpoints_plot <- plot(obj, what = "cutpoints")
  expect_s3_class(predictNMBsim_cutpoints_plot, "gg")
  vdiffr::expect_doppelganger(
    "plot.predictNMBsim(obj, what = 'cutpoints') default",
    predictNMBsim_cutpoints_plot
  )

  # test incremental net monetary benefit plot from predictNMBsim object
  predictNMBsim_inb_plot <- plot(obj, what = "inb", inb_ref_col = "all")
  expect_s3_class(predictNMBsim_inb_plot, "gg")
  vdiffr::expect_doppelganger(
    "plot.predictNMBsim(obj, what = 'inb') default",
    predictNMBsim_inb_plot
  )
})

test_that("predictNMBsim - plot method (fill_cols) works", {
  obj <- readRDS(test_path("fixtures", "predictNMBsim_object.rds"))

  # test plotting of predictNMBsim object with new fill colour choices
  predictNMBsim_christmas_plot <- plot(obj, fill_cols = c("red", "green"))
  expect_s3_class(predictNMBsim_christmas_plot, "gg")
  vdiffr::expect_doppelganger(
    "plot.predictNMBsim(obj, fill_cols = c('red', 'green'))",
    predictNMBsim_christmas_plot
  )
})


### plot.predictNMBscreen() tests

test_that("predictNMBscreen - plot method (defaults) works", {
  obj <- readRDS(test_path("fixtures", "predictNMBscreen_object.rds"))

  # test default plotting of predictNMBscreen object
  predictNMBscreen_plot <- plot(obj)
  expect_s3_class(predictNMBscreen_plot, "gg")
  vdiffr::expect_doppelganger(
    "plot.predictNMBscreen(obj)",
    predictNMBscreen_plot
  )
})

test_that("predictNMBscreen - plot method (what) works", {
  obj <- readRDS(test_path("fixtures", "predictNMBscreen_object.rds"))

  # test cutpoints plot from predictNMBsreen object
  predictNMBscreen_cutpoints_plot <- plot(obj, what = "cutpoints")
  expect_s3_class(predictNMBscreen_cutpoints_plot, "gg")
  vdiffr::expect_doppelganger(
    "plot.predictNMBscreen(obj, what = 'cutpoints')",
    predictNMBscreen_cutpoints_plot
  )

  # test incremental net monetary benefit plot from predictNMBscreen object
  predictNMBscreen_inb_plot <- plot(obj, what = "inb", inb_ref_col = "all")
  expect_s3_class(predictNMBscreen_inb_plot, "gg")
  vdiffr::expect_doppelganger(
    "plot.predictNMBscreen(obj, what = 'inb')",
    predictNMBscreen_inb_plot
  )
})

test_that("predictNMBscreen - plot method (x_axis_var selection) works", {
  obj <- readRDS(test_path("fixtures", "predictNMBscreen_object.rds"))

  # test numeric input (event_rate) being held constant
  predictNMBscreen_event_rate_plot <- plot(obj, x_axis_var = "event_rate")
  expect_s3_class(predictNMBscreen_event_rate_plot, "gg")
  vdiffr::expect_doppelganger(
    "plot.predictNMBscreen(obj, x_axis_var = 'event_rate')",
    predictNMBscreen_event_rate_plot
  )

  # test function input (fx_nmb_training) being held constant
  predictNMBscreen_fx_nmb_training_plot <- plot(
    obj,
    x_axis_var = "fx_nmb_training"
  )
  expect_s3_class(predictNMBscreen_event_rate_plot, "gg")
  vdiffr::expect_doppelganger(
    "plot.predictNMBscreen(obj, x_axis_var = 'fx_nmb_training')",
    predictNMBscreen_fx_nmb_training_plot
  )
})

test_that("predictNMBscreen - plot method (constants selection) works", {
  obj <- readRDS(test_path("fixtures", "predictNMBscreen_object.rds"))

  # test on a numeric input being held constant
  predictNMBscreen_constants_1 <- plot(
    obj,
    x_axis_var = "fx_nmb_training",
    constants = list(sim_auc = 0.8)
  )
  expect_s3_class(predictNMBscreen_constants_1, "gg")
  vdiffr::expect_doppelganger(
    "plot.predictNMBscreen - constants-1",
    predictNMBscreen_constants_1
  )

  # test on a *BAD* numeric input being held constant
  expect_error(
    plot(
      obj,
      x_axis_var = "fx_nmb_training",
      constants = list(sim_auc = 0.81)
    )
  )

  # test on a function input being held constant
  predictNMBscreen_constants_2 <- plot(
    obj,
    x_axis_var = "sim_auc",
    constants = list(fx_nmb_training = "f2")
  )
  expect_s3_class(predictNMBscreen_constants_2, "gg")
  vdiffr::expect_doppelganger(
    "plot.predictNMBscreen - constants-2",
    predictNMBscreen_constants_2
  )
})
