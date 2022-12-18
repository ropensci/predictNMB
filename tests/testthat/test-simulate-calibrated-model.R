test_that("Simulated model has approximately right AUC and is well calibrated", {

  input_auc <- 0.75

  withr::with_seed(
    42,
    training_data <- get_sample(auc = input_auc, n_samples = 5000, prevalence = 0.1)
  )

  model <- stats::glm(
    actual ~ x,
    family = stats::binomial(),
    data = training_data
  )

  training_data$preds <- stats::predict(model, type = "response")

  x <- expand.grid(
    pos = training_data[training_data$actual==1, "preds"],
    neg = training_data[training_data$actual==0, "preds"]
  )

  output_auc <- mean(x$pos > x$neg)

  # AUC is close to what was input to get_sample()
  expect_true(output_auc > input_auc*0.95, output_auc < input_auc * 1.05)

  calibration_model <- stats::lm(
    actual ~ preds,
    data = training_data
  )

  conf.ints <- confint(calibration_model, level = 0.99)

  # model is well calibrated
  expect_true(
      conf.ints[1,1] < 0 &
      conf.ints[1,2] > 0 &
      conf.ints[2,1] < 1 &
      conf.ints[2,2] > 1
  )
})
