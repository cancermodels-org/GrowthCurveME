test_that("Returns a plot - mixed-effects model", {
  data("exp_mixed_data")
  growth_model <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "mixed",
    function_type = "exponential")
  summary_growth <- summarize_growth_model(data_frame = exp_mixed_data,
                                           growth_model_object = growth_model,
                                           function_type = "exponential")
  predict_figure <- growth_model_boot_ci_curve(growth_model_object = growth_model,
                                               growth_model_summary_list = summary_growth,
                                               model_type = "mixed")
  expect_s3_class(predict_figure, "gg")
})
test_that("Returns a plot - least-squares model", {
  data("exp_mixed_data")
  growth_model <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "least-squares",
    function_type = "exponential")
  summary_growth <- summarize_growth_model(data_frame = exp_mixed_data,
                                           growth_model_object = growth_model,
                                           model_type = "least-squares",
                                           function_type = "exponential")
  predict_figure <- growth_model_boot_ci_curve(growth_model_object = growth_model,
                                               growth_model_summary_list = summary_growth,
                                               model_type = "least-squares")
  expect_s3_class(predict_figure, "gg")
})
