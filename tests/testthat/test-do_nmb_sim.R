test_that("do_nmb_sim() works", {
  get_nmb <- function() c("TP" = -3, "TN" = 0, "FP" = -1, "FN" = -4)

  out <- do_nmb_sim(
    n_sims = 10, n_valid = 1000, sim_auc = 0.7, event_rate = 0.1,
    fx_nmb_training = get_nmb, fx_nmb_evaluation = get_nmb
  )

  expect_s3_class(out, "predictNMBsim")
})


test_that("do_nmb_sim() works in parallel", {
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

  out_par <- do_nmb_sim(
    n_sims = 100, n_valid = 1000, sim_auc = 0.7, event_rate = 0.1,
    fx_nmb_training = get_nmb, fx_nmb_evaluation = get_nmb, cl = cl,
    cutpoint_methods = c("all", "none")
  )
  parallel::stopCluster(cl)
  expect_s3_class(out_par, "predictNMBsim")
})


test_that("plot method works", {
  obj <- readRDS(test_path("fixtures", "predictNMBsim_object.rds"))
  expect_s3_class(plot(obj), "gg")
})

# predictNMBsim_obj <- do_nmb_sim(
#   n_sims = 10, n_valid = 1000, sim_auc = 0.7, event_rate = 0.1,
#   fx_nmb_training = get_nmb, fx_nmb_evaluation = get_nmb
# )
# saveRDS(predictNMBsim_obj, test_path("fixtures", "predictNMBsim_object.rds"))
