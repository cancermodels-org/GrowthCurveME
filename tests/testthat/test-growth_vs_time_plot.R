test_that("Returns a plot - mixed-effects model - plot type 1", {
  data("exp_mixed_data")
  growth_model <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "mixed",
    function_type = "exponential")
  summary_growth <- summarize_growth_model(data_frame = exp_mixed_data,
                                           growth_model_object = growth_model,
                                           function_type = "exponential")
  plot <- growth_vs_time_plot(growth_model_summary_list = summary_growth,
                              model_type = "mixed",
                              plot_type = 1)
  expect_s3_class(plot, "gg")
})
test_that("Returns a plot - mixed-effects model - plot type 2", {
  data("exp_mixed_data")
  growth_model <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "mixed",
    function_type = "exponential")
  summary_growth <- summarize_growth_model(data_frame = exp_mixed_data,
                                           growth_model_object = growth_model,
                                           function_type = "exponential")
  plot <- growth_vs_time_plot(growth_model_summary_list = summary_growth,
                              model_type = "mixed",
                              plot_type = 2)
  expect_s3_class(plot, "gg")
})
test_that("Returns a plot - mixed-effects model - plot type 3", {
  data("exp_mixed_data")
  growth_model <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "mixed",
    function_type = "exponential")
  summary_growth <- summarize_growth_model(data_frame = exp_mixed_data,
                                           growth_model_object = growth_model,
                                           function_type = "exponential")
  plot <- growth_vs_time_plot(growth_model_summary_list = summary_growth,
                              model_type = "mixed",
                              plot_type = 3)
  expect_s3_class(plot, "gg")
})
test_that("Returns a plot - least-squares model - plot type 1", {
  data("exp_mixed_data")
  growth_model <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "least-squares",
    function_type = "exponential")
  summary_growth <- summarize_growth_model(data_frame = exp_mixed_data,
                                           growth_model_object = growth_model,
                                           model_type = "least-squares",
                                           function_type = "exponential")
  plot <- growth_vs_time_plot(growth_model_summary_list = summary_growth,
                              model_type = "least-squares",
                              plot_type = 1)
  expect_s3_class(plot, "gg")
})
test_that("Returns a plot - least-squares model - plot type 2", {
  data("exp_mixed_data")
  growth_model <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "least-squares",
    function_type = "exponential")
  summary_growth <- summarize_growth_model(data_frame = exp_mixed_data,
                                           growth_model_object = growth_model,
                                           model_type = "least-squares",
                                           function_type = "exponential")
  plot <- growth_vs_time_plot(growth_model_summary_list = summary_growth,
                              model_type = "least-squares",
                              plot_type = 2)
  expect_s3_class(plot, "gg")
})
test_that("Returns a plot - least-squares model - plot type 3", {
  data("exp_mixed_data")
  growth_model <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "least-squares",
    function_type = "exponential")
  summary_growth <- summarize_growth_model(data_frame = exp_mixed_data,
                                           growth_model_object = growth_model,
                                           model_type = "least-squares",
                                           function_type = "exponential")
  plot <- growth_vs_time_plot(growth_model_summary_list = summary_growth,
                              model_type = "least-squares",
                              plot_type = 3)
  expect_s3_class(plot, "gg")
})











