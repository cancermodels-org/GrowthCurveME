test_that("Returns a list object", {
  data("exp_mixed_data")
  growth_model <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "least-squares",
    function_type = "exponential",
    return_summary = FALSE)
  summary_growth_ls <- summarize_growth_model_ls(data_frame = exp_mixed_data,
                                                 ls_model  = growth_model,
                                                 function_type = "exponential")
  expect_type(summary_growth_ls, "list")
  expect_length(summary_growth_ls, 4)
})
