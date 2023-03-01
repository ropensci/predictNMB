test_that("test inbuilt cutpoint methods", {
  inbuilt_methods <- get_inbuilt_cutpoint_methods()

  nmb <- c("TP" = -3, "TN" = 0, "FP" = -1, "FN" = -4)
  p <- runif(1000)
  a <- sample(c(0, 1), size = 1000, replace = TRUE)

  for (cutpoint_method in inbuilt_methods) {
    cp <- get_inbuilt_cutpoint(
      predicted = p,
      actual = a,
      nmb = nmb,
      method = cutpoint_method
    )
    expect_true(cp >= 0 & cp <= 1, label = cutpoint_method)
  }
})
