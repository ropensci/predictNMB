### autoplot.predictNMBsim() tests

test_that("predictNMBsim - autoplot method (default) works", {
  obj <- readRDS(test_path("fixtures", "predictNMBsim_object.rds"))

  # test default plotting of predictNMBsim object
  predictNMBsim_plot <- autoplot(obj)
  expect_s3_class(predictNMBsim_plot, "gg")
  vdiffr::expect_doppelganger(
    "autoplot.predictNMBsim(obj) default",
    predictNMBsim_plot
  )
})

test_that("predictNMBsim - autoplot method with theme_sim() works", {
  obj <- readRDS(test_path("fixtures", "predictNMBsim_object.rds"))

  # test default plotting of predictNMBsim object
  predictNMBsim_plot <- autoplot(obj) + theme_sim()
  expect_s3_class(predictNMBsim_plot, "gg")
  vdiffr::expect_doppelganger(
    "autoplot.predictNMBsim(obj) + theme_sim()",
    predictNMBsim_plot
  )
})

test_that("predictNMBsim - autoplot method (what) works", {
  obj <- readRDS(test_path("fixtures", "predictNMBsim_object.rds"))

  # test cutpoints plot from predictNMBsim object
  predictNMBsim_cutpoints_plot <- autoplot(obj, what = "cutpoints")
  expect_s3_class(predictNMBsim_cutpoints_plot, "gg")
  vdiffr::expect_doppelganger(
    "autoplot.predictNMBsim(obj, what = 'cutpoints') default",
    predictNMBsim_cutpoints_plot
  )

  # test incremental net monetary benefit plot from predictNMBsim object
  predictNMBsim_inb_plot <- autoplot(obj, what = "inb", inb_ref_col = "all")
  expect_s3_class(predictNMBsim_inb_plot, "gg")
  vdiffr::expect_doppelganger(
    "autoplot.predictNMBsim(obj, what = 'inb') default",
    predictNMBsim_inb_plot
  )

  obj <- readRDS(test_path("fixtures", "predictNMBsim_ce_object.rds"))
  # test qalys plot from predictNMBsim object
  predictNMBsim_qalys_plot <- autoplot(obj, what = "qalys")
  expect_s3_class(predictNMBsim_qalys_plot, "gg")
  vdiffr::expect_doppelganger(
    "autoplot.predictNMBsim(obj, what = 'qalys') default",
    predictNMBsim_qalys_plot
  )

  # test costs plot from predictNMBsim object
  predictNMBsim_costs_plot <- autoplot(obj, what = "costs")
  expect_s3_class(predictNMBsim_costs_plot, "gg")
  vdiffr::expect_doppelganger(
    "autoplot.predictNMBsim(obj, what = 'costs') default",
    predictNMBsim_costs_plot
  )
})

test_that("predictNMBsim - autoplot method (fill_cols) works", {
  obj <- readRDS(test_path("fixtures", "predictNMBsim_object.rds"))

  # test plotting of predictNMBsim object with new fill colour choices
  predictNMBsim_christmas_plot <- autoplot(obj, fill_cols = c("red", "green"))
  expect_s3_class(predictNMBsim_christmas_plot, "gg")
  vdiffr::expect_doppelganger(
    "autoplot.predictNMBsim(obj, fill_cols = c('red', 'green'))",
    predictNMBsim_christmas_plot
  )
})


### autoplot.predictNMBscreen() tests

test_that("predictNMBscreen - autoplot method (defaults) works", {
  obj <- readRDS(test_path("fixtures", "predictNMBscreen_object.rds"))

  # test default plotting of predictNMBscreen object
  predictNMBscreen_plot <- autoplot(obj)
  expect_s3_class(predictNMBscreen_plot, "gg")
  vdiffr::expect_doppelganger(
    "autoplot.predictNMBscreen(obj)",
    predictNMBscreen_plot
  )
})

test_that("predictNMBscreen - autoplot method (defaults) works", {
  obj <- readRDS(test_path("fixtures", "predictNMBscreen_object.rds"))

  # test default plotting of predictNMBscreen object
  predictNMBscreen_plot <- autoplot(obj, dodge_width = 0.1)
  expect_s3_class(predictNMBscreen_plot, "gg")
  vdiffr::expect_doppelganger(
    "autoplot.predictNMBscreen(obj) - dodged",
    predictNMBscreen_plot
  )
})

test_that("predictNMBscreen - autoplot method (what) works", {
  obj <- readRDS(test_path("fixtures", "predictNMBscreen_object.rds"))

  # test cutpoints plot from predictNMBsreen object
  predictNMBscreen_cutpoints_plot <- autoplot(obj, what = "cutpoints")
  expect_s3_class(predictNMBscreen_cutpoints_plot, "gg")
  vdiffr::expect_doppelganger(
    "autoplot.predictNMBscreen(obj, what = 'cutpoints')",
    predictNMBscreen_cutpoints_plot
  )

  # test incremental net monetary benefit plot from predictNMBscreen object
  predictNMBscreen_inb_plot <- autoplot(obj, what = "inb", inb_ref_col = "all")
  expect_s3_class(predictNMBscreen_inb_plot, "gg")
  vdiffr::expect_doppelganger(
    "autoplot.predictNMBscreen(obj, what = 'inb')",
    predictNMBscreen_inb_plot
  )
})

test_that("predictNMBscreen - autoplot method (x_axis_var selection) works", {
  obj <- readRDS(test_path("fixtures", "predictNMBscreen_object.rds"))

  # test x_axis default selection messaging
  expect_message(
    autoplot(obj),
    "Screening over"
  )

  # test numeric input (event_rate) being held constant
  predictNMBscreen_event_rate_plot <- autoplot(obj, x_axis_var = "event_rate")
  expect_s3_class(predictNMBscreen_event_rate_plot, "gg")
  vdiffr::expect_doppelganger(
    "autoplot.predictNMBscreen(obj, x_axis_var = 'event_rate')",
    predictNMBscreen_event_rate_plot
  )

  # test function input (fx_nmb_training) being held constant
  predictNMBscreen_fx_nmb_training_plot <- autoplot(
    obj,
    x_axis_var = "fx_nmb_training"
  )
  expect_s3_class(predictNMBscreen_event_rate_plot, "gg")
  vdiffr::expect_doppelganger(
    "autoplot.predictNMBscreen(obj, x_axis_var = 'fx_nmb_train')",
    predictNMBscreen_fx_nmb_training_plot
  )
})

test_that("predictNMBscreen - autoplot method (constants selection) works-1", {
  obj <- readRDS(test_path("fixtures", "predictNMBscreen_object.rds"))
  # test on a *BAD* numeric input being held constant
  expect_error(
    autoplot(
      obj,
      x_axis_var = "fx_nmb_training",
      constants = list(sim_auc = 0.81)
    ),
    "not included in the screened inputs and cannot be used"
  )
})

test_that("predictNMBscreen - autoplot method (constants selection) works-2", {
  obj <- readRDS(test_path("fixtures", "predictNMBscreen_object.rds"))

  # test on a numeric input being held constant
  predictNMBscreen_constants_1 <- autoplot(
    obj,
    x_axis_var = "fx_nmb_training",
    constants = list(sim_auc = 0.8)
  )
  expect_s3_class(predictNMBscreen_constants_1, "gg")
  vdiffr::expect_doppelganger(
    "autoplot.predictNMBscreen - constants-1",
    predictNMBscreen_constants_1
  )



  # test on a function input being held constant
  predictNMBscreen_constants_2 <- autoplot(
    obj,
    x_axis_var = "sim_auc",
    constants = list(fx_nmb_training = "f2")
  )
  expect_s3_class(predictNMBscreen_constants_2, "gg")
  vdiffr::expect_doppelganger(
    "autoplot.predictNMBscreen - constants-2",
    predictNMBscreen_constants_2
  )
})
