test_that("Returns a plot - mixed-effects model - plot type 1", {
  data("exp_mixed_data")
  growth_model_summary <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "mixed",
    function_type = "exponential")
  plot <- growth_vs_time_plot(growth_model_summary_list = growth_model_summary,
                              plot_type = 1)
  expect_s3_class(plot, "gg")
})
test_that("Returns a plot - mixed-effects model - plot type 2", {
  data("exp_mixed_data")
  growth_model_summary <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "mixed",
    function_type = "exponential")
  plot <- growth_vs_time_plot(growth_model_summary_list = growth_model_summary,
                              plot_type = 2)
  expect_s3_class(plot, "gg")
})
test_that("Returns a plot - mixed-effects model - plot type 3", {
  data("exp_mixed_data")
  growth_model_summary <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "mixed",
    function_type = "exponential")
  plot <- growth_vs_time_plot(growth_model_summary_list = growth_model_summary,
                              plot_type = 3)
  expect_s3_class(plot, "gg")
})
test_that("Returns a plot - mixed-effects model - plot type 4", {
  data("exp_mixed_data")
  growth_model_summary <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "mixed",
    function_type = "exponential")
  plot <- growth_vs_time_plot(growth_model_summary_list = growth_model_summary,
                              plot_type = 4)
  expect_s3_class(plot, "gg")
})
test_that("Returns a plot - least-squares model - plot type 1", {
  data("exp_mixed_data")
  growth_model_summary <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "least-squares",
    function_type = "exponential")
  plot <- growth_vs_time_plot(growth_model_summary_list = growth_model_summary,
                              plot_type = 1)
  expect_s3_class(plot, "gg")
})
test_that("Returns a plot - least-squares model - plot type 2", {
  data("exp_mixed_data")
  growth_model_summary <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "least-squares",
    function_type = "exponential")
  plot <- growth_vs_time_plot(growth_model_summary_list = growth_model_summary,
                              plot_type = 2)
  expect_s3_class(plot, "gg")
})
test_that("Returns a plot - least-squares model - plot type 3", {
  data("exp_mixed_data")
  growth_model_summary <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "least-squares",
    function_type = "exponential")
  plot <- growth_vs_time_plot(growth_model_summary_list = growth_model_summary,
                              plot_type = 3)
  expect_s3_class(plot, "gg")
})
test_that("Returns a plot - least-squares model - plot type 4", {
  data("exp_mixed_data")
  growth_model_summary <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "least-squares",
    function_type = "exponential")
  plot <- growth_vs_time_plot(growth_model_summary_list = growth_model_summary,
                              plot_type = 4)
  expect_s3_class(plot, "gg")
})










