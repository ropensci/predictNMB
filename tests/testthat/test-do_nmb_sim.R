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

  out <- do_nmb_sim(
    sample_size = 100, n_sims = 10, n_valid = 1000, sim_auc = 0.7,
    event_rate = 0.1, fx_nmb_training = get_nmb, fx_nmb_evaluation = get_nmb,
    show_progress = TRUE
  )

  expect_s3_class(out, "predictNMBsim")
})

test_that("do_nmb_sim() results are similar with different seeds", {
  get_nmb <- get_nmb_sampler(
    qalys_lost = 2.5,
    wtp = 28000,
    high_risk_group_treatment_effect = 0.5,
    high_risk_group_treatment_cost = 10
  )

  withr::with_seed(
    1,
    out1 <- do_nmb_sim(
      sample_size = 50, n_sims = 500, n_valid = 1000, sim_auc = 0.7,
      event_rate = 0.3, fx_nmb_training = get_nmb, fx_nmb_evaluation = get_nmb
    )
  )

  withr::with_seed(
    2,
    out2 <- do_nmb_sim(
      sample_size = 50, n_sims = 500, n_valid = 1000, sim_auc = 0.7,
      event_rate = 0.3, fx_nmb_training = get_nmb, fx_nmb_evaluation = get_nmb
    )
  )

  expect_equal(
    colSums(out1$df_result) / 500,
    colSums(out2$df_result) / 500,
    tolerance = 1
  )

  expect_equal(
    colSums(out1$df_qalys) / 500,
    colSums(out2$df_qalys) / 500,
    tolerance = 1
  )
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

  expect_error(
    do_nmb_sim(
      sample_size = 200, n_sims = 100, n_valid = 1000, sim_auc = 0.7,
      event_rate = 0.1, fx_nmb_training = get_nmb, fx_nmb_evaluation = get_nmb,
      cl = cl, cutpoint_methods = c("f_cutpoint", "none")
    ),
    "You've included functions in in 'cutpoint_methods' which are neither"
  )
  parallel::stopCluster(cl)
})


test_that("missing method", {
  get_nmb <- function() c("TP" = -3, "TN" = 0, "FP" = -1, "FN" = -4)

  expect_error(
    do_nmb_sim(
      n_sims = 10, n_valid = 1000, sim_auc = 0.7, event_rate = 0.1, sample_size = 5,
      fx_nmb_training = get_nmb, fx_nmb_evaluation = get_nmb,
      cutpoint_methods = "f_cutpoint"
    ),
    'could not find function "f_cutpoint"'
  )
})


test_that("print method - works", {
  obj <- readRDS(test_path("fixtures", "predictNMBsim_object.rds"))
  expect_output(print.predictNMBsim(obj))
  expect_snapshot_output(print.predictNMBsim(obj))
})

# get_nmb <- function() c("TP" = -3, "TN" = 0, "FP" = -1, "FN" = -4)
# predictNMBsim_obj <- do_nmb_sim(
#   n_sims = 10, n_valid = 1000, sim_auc = 0.7, event_rate = 0.1,
#   fx_nmb_training = get_nmb, fx_nmb_evaluation = get_nmb
# )
# saveRDS(predictNMBsim_obj, test_path("fixtures", "predictNMBsim_object.rds"))
