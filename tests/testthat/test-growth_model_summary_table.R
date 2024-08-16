test_that("Returns a flextable for mixed-effects model summary object", {
  data("exp_mixed_data")
  growth_model <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "mixed",
    function_type = "exponential")
  summary_growth <- summarize_growth_model(data_frame = exp_mixed_data,
                                           growth_model_object = growth_model,
                                           function_type = "exponential")
  flx_tbl <- growth_model_summary_table(summary_growth)
  expect_s3_class(flx_tbl, "flextable")
})
test_that("Returns a flextable for least-squares model summary object", {
  data("exp_mixed_data")
  growth_model <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "least-squares",
    function_type = "exponential")
  summary_growth <- summarize_growth_model(data_frame = exp_mixed_data,
                                           growth_model_object = growth_model,
                                           model_type = "least-squares",
                                           function_type = "exponential")
  flx_tbl <- growth_model_summary_table(summary_growth)
  expect_s3_class(flx_tbl, "flextable")
})
