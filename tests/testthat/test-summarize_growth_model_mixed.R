test_that("Returns a list object", {
  data("exp_mixed_data")
  growth_model <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "mixed",
    function_type = "exponential",
    return_summary = FALSE)
  summary_growth_m <- summarize_growth_model_mixed(data_frame = exp_mixed_data,
                                                   mixed_growth_model  = growth_model,
                                                   function_type = "exponential")
  expect_type(summary_growth_m, "list")
  expect_length(summary_growth_m, 4)
})
