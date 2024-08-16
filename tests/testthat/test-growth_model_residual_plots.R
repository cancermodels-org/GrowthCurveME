test_that("Returns a plot - marginal residuals of mixed-effects model", {
  data("exp_mixed_data")
  growth_model <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "mixed",
    function_type = "exponential")
  summary_growth <- summarize_growth_model(data_frame = exp_mixed_data,
                                           growth_model_object = growth_model,
                                           function_type = "exponential")
  res_plot <- growth_model_residual_plots(summary_growth,
                                          model_type = "mixed",
                                          residual_type = "marginal")
  expect_s3_class(res_plot, "patchwork")
})
test_that("Returns a plot - conditional residuals of mixed-effects model", {
  data("exp_mixed_data")
  growth_model <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "mixed",
    function_type = "exponential")
  summary_growth <- summarize_growth_model(data_frame = exp_mixed_data,
                                           growth_model_object = growth_model,
                                           function_type = "exponential")
  res_plot <- growth_model_residual_plots(summary_growth,
                                          model_type = "mixed",
                                          residual_type = "conditional")
  expect_s3_class(res_plot, "patchwork")
})
test_that("Returns a plot - marginal residuals of least-squares model", {
  data("exp_mixed_data")
  growth_model <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "least-squares",
    function_type = "exponential")
  summary_growth <- summarize_growth_model(data_frame = exp_mixed_data,
                                           growth_model_object = growth_model,
                                           model_type = "least-squares",
                                           function_type = "exponential")
  res_plot <- growth_model_residual_plots(summary_growth,
                                          model_type = "least-squares",
                                          residual_type = "marginal")
  expect_s3_class(res_plot, "patchwork")
})
test_that("Return a message for when conditional is specified for a least-squares model by mistake", {
  data("exp_mixed_data")
  growth_model <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "least-squares",
    function_type = "exponential")
  summary_growth <- summarize_growth_model(data_frame = exp_mixed_data,
                                           growth_model_object = growth_model,
                                           model_type = "least-squares",
                                           function_type = "exponential")
  expect_message(growth_model_residual_plots(summary_growth,
                                             model_type = "least-squares",
                                             residual_type = "conditional"))
})




