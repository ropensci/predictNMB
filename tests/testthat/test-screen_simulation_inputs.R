test_that("screen_simulation_inputs() works", {
  get_nmb <- function() c("TP" = -3, "TN" = 0, "FP" = -1, "FN" = -4)
  sim_screen_obj <- screen_simulation_inputs(
    n_sims = 10, n_valid = 1000, sim_auc = seq(0.7, 0.9, 0.1), event_rate = 0.1,
    fx_nmb_training = get_nmb, fx_nmb_evaluation = get_nmb
  )
  expect_s3_class(sim_screen_obj, "predictNMBscreen")
})

test_that("screen_simulation_inputs() with paired functions works", {
  get_nmb1 <- function() c("TP" = -3, "TN" = 0, "FP" = -1, "FN" = -4)
  get_nmb2 <- function() c("TP" = -5, "TN" = 0, "FP" = -1.5, "FN" = -2)

  sim_screen_obj <- screen_simulation_inputs(
    sample_size = 100,
    n_sims = 10, n_valid = 1000, sim_auc = c(0.7, 0.8), event_rate = 0.1,
    fx_nmb_training = list(get_nmb1, get_nmb2),
    fx_nmb_evaluation = list(get_nmb1, get_nmb2),
    pair_nmb_train_and_evaluation_functions = TRUE
  )

  expect_equal(nrow(sim_screen_obj$input_grid), 2 * 2)

  sim_screen_obj <- screen_simulation_inputs(
    sample_size = 100,
    n_sims = 10, n_valid = 1000, sim_auc = c(0.7, 0.8), event_rate = 0.1,
    fx_nmb_training = list(get_nmb1, get_nmb2),
    fx_nmb_evaluation = list(get_nmb1, get_nmb2),
    pair_nmb_train_and_evaluation_functions = FALSE
  )

  expect_equal(nrow(sim_screen_obj$input_grid), 2 * 2 * 2)
})


test_that("screen_simulation_inputs() works in parallel", {
  get_nmb <- function() c("TP" = -3, "TN" = 0, "FP" = -1, "FN" = -4)
  if (!requireNamespace("parallel", quietly = TRUE)) {
    skip()
  }

  chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
  if (nzchar(chk) && chk == TRUE) {
    ncores <- 2
  } else {
    ncores <- parallel::detectCores()
  }
  cl <- parallel::makeCluster(ncores)

  sim_screen_obj_par <- screen_simulation_inputs(
    sample_size = 250, n_sims = 10, n_valid = 1000,
    sim_auc = seq(0.7, 0.9, 0.1), event_rate = 0.1,
    fx_nmb_training = get_nmb, fx_nmb_evaluation = get_nmb, cl = cl
  )

  expect_s3_class(sim_screen_obj_par, "predictNMBscreen")


  if (!requireNamespace("pbapply", quietly = TRUE)) {
    parallel::stopCluster(cl)
    skip()
  }

  sim_screen_obj_par_progress <- screen_simulation_inputs(
    sample_size = 250, n_sims = 10, n_valid = 1000,
    sim_auc = seq(0.7, 0.9, 0.1), event_rate = 0.1,
    fx_nmb_training = get_nmb, fx_nmb_evaluation = get_nmb, cl = cl,
    show_progress = TRUE
  )

  expect_s3_class(sim_screen_obj_par_progress, "predictNMBscreen")

  parallel::stopCluster(cl)
})

test_that("print method - works", {
  obj <- readRDS(test_path("fixtures", "predictNMBscreen_object.rds"))
  expect_output(print.predictNMBscreen(obj))
})


# cl <- parallel::makeCluster(parallel::detectCores())
# get_nmb <- function() c("TP" = -3, "TN" = 0, "FP" = -1, "FN" = -4)
# sim_screen_obj_par <- screen_simulation_inputs(
#   n_sims = 500, n_valid = 1000, sim_auc = seq(0.7, 0.9, 0.1),
#   event_rate = 0.1, fx_nmb_training = list("f1"=get_nmb, "f2"=get_nmb),
#   fx_nmb_evaluation = get_nmb, cl = cl
# )
# saveRDS(sim_screen_obj_par, test_path("fixtures", "predictNMBscreen_object.rds"))
# parallel::stopCluster(cl)
