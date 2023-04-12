test_that("do_nmb_sim() works", {
  get_nmb <- function() c("TP" = -3, "TN" = 0, "FP" = -1, "FN" = -4)

  out <- do_nmb_sim(
    n_sims = 10, n_valid = 1000, sim_auc = 0.7, event_rate = 0.1,
    fx_nmb_training = get_nmb, fx_nmb_evaluation = get_nmb
  )

  expect_s3_class(out, "predictNMBsim")
  expect_equal(nrow(out$df_result), nrow(stats::na.omit(out$df_result)))
  expect_equal(nrow(out$df_thresholds), nrow(stats::na.omit(out$df_thresholds)))

  out <- do_nmb_sim(
    sample_size = 100, n_sims = 10, n_valid = 1000, sim_auc = 0.7,
    event_rate = 0.1, fx_nmb_training = get_nmb, fx_nmb_evaluation = get_nmb
  )

  expect_s3_class(out, "predictNMBsim")
})


test_that("do_nmb_sim() results are similar with different seeds", {
  get_nmb <- function() c("TP" = -3, "TN" = 0, "FP" = -1, "FN" = -4)

  withr::with_seed(
    1,
    out1 <- do_nmb_sim(
      sample_size = 500, n_sims = 500, n_valid = 1000, sim_auc = 0.7,
      event_rate = 0.3, fx_nmb_training = get_nmb, fx_nmb_evaluation = get_nmb
    )
  )

  withr::with_seed(
    2,
    out2 <- do_nmb_sim(
      sample_size = 500, n_sims = 500, n_valid = 1000, sim_auc = 0.7,
      event_rate = 0.3, fx_nmb_training = get_nmb, fx_nmb_evaluation = get_nmb
    )
  )

  result_differences <- abs(
    colMeans(out1$df_result)[-1] - colMeans(out2$df_result)[-1]
  )
  result_tolerances <- abs(colMeans(out1$df_result) * 0.1)[-1]

  expect_true(all(result_differences < result_tolerances))

  thresholds_differences <- abs(
    colMeans(out1$df_thresholds)[-c(1:3)] -
      colMeans(out2$df_thresholds)[-c(1:3)]
  )
  thresholds_tolerances <- abs(colMeans(out1$df_thresholds)[-c(1:3)] * 0.1)

  expect_true(all(thresholds_differences < thresholds_tolerances))
})

test_that("do_nmb_sim() throws error for bad inputs", {
  get_nmb <- function() c("TP" = -3, "TN" = 0, "FP" = -1, "FN" = -4)

  f <- function() {
    do_nmb_sim(
      sample_size = 100,
      n_sims = c(10, 20),
      n_valid = c(1000, 5),
      sim_auc = c(0.7, 0.9),
      event_rate = 0.1,
      fx_nmb_training = get_nmb,
      fx_nmb_evaluation = get_nmb
    )
  }

  expect_error(f()) # multiple inputs

  f <- function() {
    do_nmb_sim(
      sample_size = 100.5,
      n_sims = 100,
      n_valid = 1000,
      sim_auc = 0.7,
      event_rate = 0.1,
      fx_nmb_training = get_nmb,
      fx_nmb_evaluation = get_nmb
    )
  }

  expect_error(f()) # non-integer for sample_size

  f <- function() {
    do_nmb_sim(
      sample_size = 100,
      n_sims = 5.5,
      n_valid = 1000,
      sim_auc = 0.7,
      event_rate = 0.1,
      fx_nmb_training = get_nmb,
      fx_nmb_evaluation = get_nmb
    )
  }

  expect_error(f()) # non-integer for n_sims

  f <- function() {
    do_nmb_sim(
      sample_size = 100,
      n_sims = 100,
      n_valid = 100.5,
      sim_auc = 0.7,
      event_rate = 0.1,
      fx_nmb_training = get_nmb,
      fx_nmb_evaluation = get_nmb
    )
  }

  expect_error(f()) # non-integer for n_valid

  f <- function() {
    do_nmb_sim(
      sample_size = 100,
      n_sims = 100,
      n_valid = 1000,
      sim_auc = 1.1,
      event_rate = 0.1,
      fx_nmb_training = get_nmb,
      fx_nmb_evaluation = get_nmb
    )
  }

  expect_error(f()) # sim_auc out of range

  f <- function() {
    do_nmb_sim(
      sample_size = 100,
      n_sims = 100,
      n_valid = 1000,
      sim_auc = 0.7,
      event_rate = 1.1,
      fx_nmb_training = get_nmb,
      fx_nmb_evaluation = get_nmb
    )
  }

  expect_error(f()) # event_rate out of range

  f <- function() {
    do_nmb_sim(
      sample_size = 100,
      n_sims = 100,
      n_valid = 1000,
      sim_auc = 0.7,
      event_rate = 1.1,
      fx_nmb_training = get_nmb,
      fx_nmb_evaluation = get_nmb
    )
  }

  expect_error(f()) # event_rate out of range

  f <- function() {
    do_nmb_sim(
      sample_size = 100,
      n_sims = 100,
      n_valid = 1000,
      sim_auc = 0.7,
      event_rate = 0.1,
      fx_nmb_training = "get_nmb",
      fx_nmb_evaluation = get_nmb
    )
  }

  expect_error(f()) # fx_nmb_training not a function

  f <- function() {
    do_nmb_sim(
      sample_size = 100,
      n_sims = 100,
      n_valid = 1000,
      sim_auc = 0.7,
      event_rate = 0.1,
      fx_nmb_training = get_nmb,
      fx_nmb_evaluation = "get_nmb"
    )
  }

  expect_error(f()) # fx_nmb_evaluation not a function

  f <- function() {
    do_nmb_sim(
      sample_size = 100,
      n_sims = c(),
      n_valid = 1000,
      sim_auc = 0.7,
      event_rate = 0.1,
      fx_nmb_training = get_nmb,
      fx_nmb_evaluation = get_nmb
    )
  }

  expect_error(f()) # zero length data throws error

  f <- function() {
    do_nmb_sim(
      n_sims = 5,
      n_valid = 1000,
      sim_auc = 0.7,
      event_rate = 0.1,
      min_events = 5,
      meet_min_events = TRUE,
      fx_nmb_training = get_nmb,
      fx_nmb_evaluation = get_nmb
    )
  }

  # expect to get message for specifying min_events when
  # sample size is being calculated by power analyses
  expect_message(f(), regexp = "Power analyses")

  f <- function() {
    do_nmb_sim(
      sample_size = 100,
      n_sims = 5,
      n_valid = 1000,
      sim_auc = 0.7,
      event_rate = 0.1,
      min_events = 5,
      meet_min_events = FALSE,
      fx_nmb_training = get_nmb,
      fx_nmb_evaluation = get_nmb
    )
  }

  # expect to get message for specifying min_events but meet_min_events = FALSE
  expect_message(f(), regexp = "'min_events' will be ignored")
})

test_that("do_nmb_sim() works in parallel", {
  get_nmb <- function() c("TP" = -3, "TN" = 0, "FP" = -1, "FN" = -4)
  if (!requireNamespace("parallel", quietly = TRUE)) {
    skip("parallel tests skipped as parallel is not installed")
  }

  ncores <- 2
  cl <- parallel::makeCluster(ncores)

  out_par <- do_nmb_sim(
    sample_size = 200, n_sims = 100, n_valid = 1000, sim_auc = 0.7,
    event_rate = 0.1, fx_nmb_training = get_nmb, fx_nmb_evaluation = get_nmb,
    cl = cl, cutpoint_methods = c("all", "none")
  )

  expect_s3_class(out_par, "predictNMBsim")

  if (!requireNamespace("pbapply", quietly = TRUE)) {
    parallel::stopCluster(cl)
    skip("progress bar tests skipped as pbapply is not installed")
  }

  out_par_progress <- do_nmb_sim(
    sample_size = 200, n_sims = 100, n_valid = 1000, sim_auc = 0.7,
    event_rate = 0.1, fx_nmb_training = get_nmb, fx_nmb_evaluation = get_nmb,
    cl = cl, cutpoint_methods = c("all", "none"), show_progress = TRUE
  )

  expect_s3_class(out_par_progress, "predictNMBsim")

  parallel::stopCluster(cl)
})

test_that("print method - works", {
  obj <- readRDS(test_path("fixtures", "predictNMBsim_object.rds"))
  expect_output(print.predictNMBsim(obj))
  expect_snapshot_output(print.predictNMBsim(obj))
})


# predictNMBsim_obj <- do_nmb_sim(
#   n_sims = 10, n_valid = 1000, sim_auc = 0.7, event_rate = 0.1,
#   fx_nmb_training = get_nmb, fx_nmb_evaluation = get_nmb
# )
# saveRDS(predictNMBsim_obj, test_path("fixtures", "predictNMBsim_object.rds"))
