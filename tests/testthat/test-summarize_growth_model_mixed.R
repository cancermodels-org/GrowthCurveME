test_that("Returns a list object", {
  data("exp_mixed_data")
  growth_model <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "mixed",
    function_type = "exponential")
  summary_growth_m <- summarize_growth_model_mixed(data_frame = exp_mixed_data,
                                                   mixed_growth_model  = growth_model,
                                                   function_type = "exponential")
  expect_type(summary_growth_m, "list")
})
test_that("Returns the same list object as summarize_growth_model_mixed", {
  data("exp_mixed_data")
  growth_model <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "mixed",
    function_type = "exponential")
  summary_growth <- summarize_growth_model(data_frame = exp_mixed_data,
                                           growth_model_object = growth_model,
                                           function_type = "exponential")
  summary_growth_m <- summarize_growth_model_mixed(data_frame = exp_mixed_data,
                                                   mixed_growth_model  = growth_model,
                                                   function_type = "exponential")
  expect_equal(length(summary_growth), length(summary_growth_m))
  expect_equal(names(summary_growth), names(summary_growth_m))
})
