### summary.predictNMBsim() tests
test_that("predictNMBsim - summary (default) works", {
  obj <- readRDS(test_path("fixtures", "predictNMBsim_object.rds"))

  # test default summary for predictNMBsim object
  predictNMBsim_default_tbl <- summary(obj)
  expect_s3_class(predictNMBsim_default_tbl, "tbl")
  expect_snapshot_output(predictNMBsim_default_tbl)
  expect_true(all(dim(predictNMBsim_default_tbl) == c(8, 3)))
})

test_that("predictNMBsim - summary (rename_vector) works", {
  obj <- readRDS(test_path("fixtures", "predictNMBsim_object.rds"))

  # test that rename_vector works for summary.predictNMBsim()
  predictNMBsim_renamed_tbl <- summary(
    obj,
    rename_vector = c("Treat All" = "all", "Treat None" = "none")
  )

  expect_s3_class(predictNMBsim_renamed_tbl, "tbl")
  expect_snapshot_output(predictNMBsim_renamed_tbl)
})

test_that("predictNMBsim - summary (what) works", {
  obj <- readRDS(test_path("fixtures", "predictNMBsim_object.rds"))

  # test that cutpoints works for summary.predictNMBsim()
  predictNMBsim_cutpoints_tbl <- summary(
    obj,
    what = "cutpoints"
  )

  expect_s3_class(predictNMBsim_cutpoints_tbl, "tbl")
  expect_snapshot_output(predictNMBsim_cutpoints_tbl)

})

test_that("predictNMBsim - summary (what) works", {
  obj <- readRDS(test_path("fixtures", "predictNMBsim_object.rds"))

  # test that incremental net monetary benefit works for
  # summary.predictNMBsim()
  predictNMBsim_inb_tbl <- summary(
    obj,
    what = "inb",
    inb_ref_col = "all"
  )

  expect_s3_class(predictNMBsim_inb_tbl, "tbl")
  expect_snapshot_output(predictNMBsim_inb_tbl)
})


test_that("predictNMBsim - summary (agg_functions) works", {
  obj <- readRDS(test_path("fixtures", "predictNMBsim_object.rds"))

  # test that agg_functions works for summary.predictNMBsim()
  predictNMBsim_custom_fx_tbl <- summary(
    obj,
    agg_functions = list(mean = mean, min = min, max = max)
  )

  expect_s3_class(predictNMBsim_custom_fx_tbl, "tbl")
  expect_snapshot_output(predictNMBsim_custom_fx_tbl)
})


### summary.predictNMBscreen() tests
test_that("predictNMBscreen - summary works", {
  # these tests are simpler than the predictNMBsim tests above because
  # both methods just call get_sim_data() and lapply the agg_functions to them.

  obj <- readRDS(test_path("fixtures", "predictNMBscreen_object.rds"))

  # test default summary for predictNMBscreen object
  predictNMBscreen_default_tbl <- summary(obj)
  expect_s3_class(predictNMBscreen_default_tbl, "tbl")
  expect_snapshot_output(as.data.frame(predictNMBscreen_default_tbl))
  expect_true(all(dim(predictNMBscreen_default_tbl) == c(6, 18)))

  # test a more complex case
  predictNMBscreen_complex_1 <- summary(
    obj,
    rename_vector = c(
      "Treat All" = "all",
      "Treat None" = "none",
      "cost minimising threshold" = "cost minimising"
    ),
    agg_functions = list(mean = mean, min = min, max = max)
  )
  expect_s3_class(predictNMBscreen_complex_1, "tbl")
  expect_snapshot_output(as.data.frame(predictNMBscreen_complex_1))
  expect_equal(ncol(predictNMBscreen_default_tbl), 18)
})

test_that("predictNMBscreen - summary works", {
  obj <- readRDS(test_path("fixtures", "predictNMBscreen_object.rds"))
  # test that show_full_inputs returns the wider dataset with simulation inputs
  tbl_with_full_inputs <- summary(obj, show_full_inputs = TRUE)
  expect_s3_class(tbl_with_full_inputs, "tbl")
  expect_snapshot_output(as.data.frame(tbl_with_full_inputs))
  expect_equal(ncol(tbl_with_full_inputs), 26)
})
