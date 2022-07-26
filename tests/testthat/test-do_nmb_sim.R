test_that("do_nmb_sim() works", {
  get_nmb <- function() c("TP" = -3, "TN" = 0, "FP" = -1, "FN" = -4)

  out <- do_nmb_sim(
    n_sims = 10, n_valid = 1000, sim_auc = 0.7, event_rate = 0.1,
    fx_nmb_training = get_nmb, fx_nmb_evaluation = get_nmb
  )

  expect_s3_class(out, "predictNMBsim")
})


test_that("plot method works", {
  obj <- readRDS(test_path("fixtures", "predictNMBsim_object.rds"))
  expect_s3_class(plot(obj), "gg")
})
