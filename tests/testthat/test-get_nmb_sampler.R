test_that("get_nmb_sampler() works when used with do_nmb_sim() in parallel", {

  get_nmb_train <- get_nmb_sampler(outcome_cost = function() rgamma(1, 1000, 10),
                                   wtp=28000,
                                   qalys_lost=function() rgamma(1, 1),
                                   high_risk_group_treatment_cost = 50,
                                   high_risk_group_treatment_effect = function() rbeta(1, 10, 20),
                                   use_expected_values = TRUE)

  get_nmb_eval <- get_nmb_sampler(outcome_cost = function() rgamma(1, 1000, 10),
                                  wtp=28000,
                                  qalys_lost=function() rgamma(1, 1),
                                  high_risk_group_treatment_cost = 50,
                                  high_risk_group_treatment_effect = function() rbeta(1, 10, 20))

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
    n_sims = 50, sample_size = 100, min_events = 10, n_valid = 1000, sim_auc = 0.7, event_rate = 0.1,
    fx_nmb_training = get_nmb_train, fx_nmb_evaluation = get_nmb_eval, cl = cl,
    cutpoint_methods = c("all", "none")
  )
  parallel::stopCluster(cl)
  expect_s3_class(out_par, "predictNMBsim")
})
