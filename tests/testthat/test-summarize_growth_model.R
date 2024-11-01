test_that("Returns a list object of length 3", {
  data("exp_mixed_data")
  growth_model <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "mixed",
    function_type = "exponential",
    return_summary = FALSE)
  summary_growth <- summarize_growth_model(data_frame = exp_mixed_data,
                                           growth_model_object = growth_model,
                                           function_type = "exponential")
  expect_type(summary_growth, "list")
  expect_length(summary_growth, 4)
})
test_that("Returns the same list object as summarize_growth_model_mixed", {
  data("exp_mixed_data")
  growth_model <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "mixed",
    function_type = "exponential",
    return_summary = FALSE)
  summary_growth <- summarize_growth_model(data_frame = exp_mixed_data,
                                           growth_model_object = growth_model,
                                           function_type = "exponential")
  summary_growth_m <- summarize_growth_model_mixed(data_frame = exp_mixed_data,
                                                  mixed_growth_model  = growth_model,
                                                  function_type = "exponential")
  expect_equal(length(summary_growth), length(summary_growth_m))
  expect_equal(names(summary_growth), names(summary_growth_m))
})
test_that("Returns the same list object as summarize_growth_model_ls", {
  data("exp_mixed_data")
  growth_model <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "least-squares",
    function_type = "exponential",
    return_summary = FALSE)
  summary_growth <- summarize_growth_model(data_frame = exp_mixed_data,
                                           growth_model_object = growth_model,
                                           model_type = "least-squares",
                                           function_type = "exponential")
  summary_growth_ls <- summarize_growth_model_ls(data_frame = exp_mixed_data,
                                                   ls_model  = growth_model,
                                                   function_type = "exponential")

  expect_equal(length(summary_growth), length(summary_growth_ls))
  expect_equal(names(summary_growth), names(summary_growth_ls))
})
