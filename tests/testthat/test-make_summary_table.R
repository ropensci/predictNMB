### make_summary_table.predictNMBsim() tests
test_that("predictNMBsim - make_summary_table (default) works", {
  obj <- readRDS(test_path("fixtures", "predictNMBsim_object.rds"))

  # test default make_summary_table for predictNMBsim object
  predictNMBsim_default_tbl <- make_summary_table(obj)
  expect_s3_class(predictNMBsim_default_tbl, "tbl")
  expect_snapshot_output(predictNMBsim_default_tbl)
  expect_true(all(dim(predictNMBsim_default_tbl) == c(8, 3)))
})

test_that("predictNMBsim - make_summary_table (rename_vector) works", {
  obj <- readRDS(test_path("fixtures", "predictNMBsim_object.rds"))

  # test that rename_vector works for make_summary_table.predictNMBsim()
  predictNMBsim_renamed_tbl <- make_summary_table(
    obj,
    rename_vector = c("Treat All" = "all", "Treat None" = "none")
  )

  expect_s3_class(predictNMBsim_renamed_tbl, "tbl")
  expect_snapshot_output(predictNMBsim_renamed_tbl)
})

test_that("predictNMBsim - make_summary_table (what) works", {
  obj <- readRDS(test_path("fixtures", "predictNMBsim_object.rds"))

  # test that cutpoints works for make_summary_table.predictNMBsim()
  predictNMBsim_cutpoints_tbl <- make_summary_table(
    obj,
    what = "cutpoints"
  )

  expect_s3_class(predictNMBsim_cutpoints_tbl, "tbl")
  expect_snapshot_output(predictNMBsim_cutpoints_tbl)

  # test that incremental net monetary benefit works for
  # make_summary_table.predictNMBsim()
  predictNMBsim_inb_tbl <- make_summary_table(
    obj,
    what = "inb",
    inb_ref_col = "all"
  )

  expect_s3_class(predictNMBsim_inb_tbl, "tbl")
  expect_snapshot_output(predictNMBsim_inb_tbl)
})


test_that("predictNMBsim - make_summary_table (agg_functions) works", {
  obj <- readRDS(test_path("fixtures", "predictNMBsim_object.rds"))

  # test that agg_functions works for make_summary_table.predictNMBsim()
  predictNMBsim_custom_fx_tbl <- make_summary_table(
    obj,
    agg_functions = list(mean = mean, min = min, max = max)
  )

  expect_s3_class(predictNMBsim_custom_fx_tbl, "tbl")
  expect_snapshot_output(predictNMBsim_custom_fx_tbl)
})


### make_summary_table.predictNMBscreen() tests
test_that("predictNMBscreen - make_summary_table works", {
  # these tests are simpler than the predictNMBsim tests above because
  # both methods just call get_sim_data() and lapply the agg_functions to them.

  obj <- readRDS(test_path("fixtures", "predictNMBscreen_object.rds"))

  # test default make_summary_table for predictNMBscreen object
  predictNMBscreen_default_tbl <- make_summary_table(obj)
  expect_s3_class(predictNMBscreen_default_tbl, "tbl")
  expect_snapshot_output(predictNMBscreen_default_tbl)
  expect_true(all(dim(predictNMBscreen_default_tbl) == c(6, 18)))

  # test a more complex case
  predictNMBscreen_complex_1 <- make_summary_table(
    obj,
    rename_vector = c("Treat All" = "all",
                      "Treat None" = "none",
                      "cost minimising threshold" = "cost minimising"),
    agg_functions = list(mean = mean, min = min, max = max)
  )
  expect_s3_class(predictNMBscreen_complex_1, "tbl")
  expect_snapshot_output(predictNMBscreen_complex_1)

  # test that show_full_inputs returns the wider dataset with simulation inputs
  tbl_with_full_inputs <- make_summary_table(obj, show_full_inputs = TRUE)
  expect_s3_class(tbl_with_full_inputs, "tbl")
  expect_snapshot_output(tbl_with_full_inputs)
  expect_gt(ncol(tbl_with_full_inputs), ncol(predictNMBscreen_default_tbl))
})
