test_that("ce_plot works", {
  get_nmb_training <- get_nmb_sampler(
    qalys_lost = function() rnorm(1, 0.33, 0.03),
    wtp=28000,
    high_risk_group_treatment_effect = function() exp(rnorm(n = 1, mean = log(0.58), sd = 0.43)),
    high_risk_group_treatment_cost = function() rnorm(n = 1, mean = 161, sd = 49),
    use_expected_values = TRUE
  )
  get_nmb_evaluation <- get_nmb_sampler(
    qalys_lost = function() rnorm(1, 0.33, 0.03),
    wtp=28000,
    high_risk_group_treatment_effect = function() exp(rnorm(n = 1, mean = log(0.58), sd = 0.43)),
    high_risk_group_treatment_cost = function() rnorm(n = 1, mean = 161, sd = 49)
  )
  sim_obj <- readRDS(test_path("fixtures", "predictNMBsim_ce_object.rds"))

  sim_ce_plot <- ce_plot(sim_obj, ref_col = "all")
  expect_s3_class(sim_ce_plot, "gg")
  vdiffr::expect_doppelganger(
    "ce_plot.predictNMBsim(obj) all",
    sim_ce_plot
  )

  sim_ce_plot <- ce_plot(sim_obj, ref_col = "none")
  expect_s3_class(sim_ce_plot, "gg")
  vdiffr::expect_doppelganger(
    "ce_plot.predictNMBsim(obj) none",
    sim_ce_plot
  )

  sim_ce_plot <- ce_plot(sim_obj, ref_col = "none", wtp = 28000)
  expect_no_message(ce_plot(sim_obj, ref_col = "none", wtp = 28000))
  expect_message(
    ce_plot(sim_obj, ref_col = "none", wtp = 1),
    "misinterpretation"
  )

  expect_error(
    ce_plot(sim_obj),
    "'ref_col' must be specified"
  )

  vdiffr::expect_doppelganger(
    "ce_plot.predictNMBsim(obj) no-WTP-line",
    ce_plot(sim_obj, ref_col = "none", show_wtp = FALSE)
  )

  vdiffr::expect_doppelganger(
    "ce_plot.predictNMBsim(obj) methods_reorder",
    ce_plot(
      sim_obj,
      ref_col = "none",
      methods_order = c("all", "youden", "value optimising")
    )
  )

  get_nmb_evaluation <- get_nmb_sampler(
    qalys_lost = function() rnorm(1, 0.33, 0.03),
    wtp = function() rnorm(1, 28000, 1000),
    high_risk_group_treatment_effect = function() exp(rnorm(n = 1, mean = log(0.58), sd = 0.43)),
    high_risk_group_treatment_cost = function() rnorm(n = 1, mean = 161, sd = 49)
  )

  sim_obj <- readRDS(test_path("fixtures", "predictNMBsim_ce_uncertain-wtp_object.rds"))
  expect_message(
    ce_plot(sim_obj, ref_col = "none"),
    "10000 samples"
  )

  sim_obj <- readRDS(test_path("fixtures", "predictNMBsim_object.rds"))
  expect_error(
    ce_plot(sim_obj, ref_col = "none"),
    "cost-effectiveness plot cannot be made"
  )
})

# get_nmb_training <- get_nmb_sampler(
#   qalys_lost = function() rnorm(1, 0.33, 0.03),
#   wtp=28000,
#   high_risk_group_treatment_effect = function() exp(rnorm(n = 1, mean = log(0.58), sd = 0.43)),
#   high_risk_group_treatment_cost = function() rnorm(n = 1, mean = 161, sd = 49),
#   use_expected_values = TRUE
# )
# get_nmb_evaluation <- get_nmb_sampler(
#   qalys_lost = function() rnorm(1, 0.33, 0.03),
#   wtp=28000,
#   high_risk_group_treatment_effect = function() exp(rnorm(n = 1, mean = log(0.58), sd = 0.43)),
#   high_risk_group_treatment_cost = function() rnorm(n = 1, mean = 161, sd = 49)
# )
# predictNMBsim_obj <- do_nmb_sim(
#   n_sims = 100, n_valid = 1000, sim_auc = 0.7, event_rate = 0.1,
#   fx_nmb_training = get_nmb_training, fx_nmb_evaluation = get_nmb_evaluation
# )
# saveRDS(predictNMBsim_obj, test_path("fixtures", "predictNMBsim_ce_object.rds"))
# get_nmb_evaluation <- get_nmb_sampler(
#   qalys_lost = function() rnorm(1, 0.33, 0.03),
#   wtp = function() rnorm(1, 28000, 1000),
#   high_risk_group_treatment_effect = function() exp(rnorm(n = 1, mean = log(0.58), sd = 0.43)),
#   high_risk_group_treatment_cost = function() rnorm(n = 1, mean = 161, sd = 49)
# )
# predictNMBsim_obj <- do_nmb_sim(
#   n_sims = 100, n_valid = 1000, sim_auc = 0.7, event_rate = 0.1,
#   fx_nmb_training = get_nmb_training, fx_nmb_evaluation = get_nmb_evaluation
# )
# saveRDS(predictNMBsim_obj, test_path("fixtures", "predictNMBsim_ce_uncertain-wtp_object.rds"))
