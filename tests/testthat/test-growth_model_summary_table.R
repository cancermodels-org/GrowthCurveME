test_that("Returns a flextable for mixed-effects model summary object", {
  data("exp_mixed_data")
  growth_model_summary <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "mixed",
    function_type = "exponential")
  flx_tbl <- growth_model_summary_table(
    growth_model_summary_list = growth_model_summary)
  expect_s3_class(flx_tbl, "flextable")
})
test_that("Returns a flextable for least-squares model summary object", {
  data("exp_mixed_data")
  growth_model_summary <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "least-squares",
    function_type = "exponential")
  flx_tbl <- growth_model_summary_table(growth_model_summary)
  expect_s3_class(flx_tbl, "flextable")
})
