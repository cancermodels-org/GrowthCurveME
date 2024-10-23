test_that("Returns a plot - mixed-effects population level weighted
          residuals", {
  data("exp_mixed_data")
  growth_model_summary <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "mixed",
    function_type = "exponential")
  res_plot <- growth_model_residual_plots(growth_model_summary,
                                          residual_type = "population")
  expect_s3_class(res_plot, "patchwork")
})
test_that("Returns a plot - mixed-effects population level raw
          residuals", {
  data("exp_mixed_data")
  growth_model_summary <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "mixed",
    function_type = "exponential")
  res_plot <- growth_model_residual_plots(growth_model_summary,
                                          residual_type = "population",
                                          weighted = FALSE)
  expect_s3_class(res_plot, "patchwork")
})
test_that("Returns a plot - mixed-effects cluster level weighted
          residuals", {
  data("exp_mixed_data")
  growth_model_summary <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "mixed",
    function_type = "exponential")
  res_plot <- growth_model_residual_plots(growth_model_summary,
                                          residual_type = "cluster")
  expect_s3_class(res_plot, "patchwork")
})
test_that("Returns a plot - mixed-effects cluster level raw
          residuals", {
  data("exp_mixed_data")
  growth_model_summary <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "mixed",
    function_type = "exponential")
  res_plot <- growth_model_residual_plots(growth_model_summary,
                                          residual_type = "cluster",
                                          weighted = FALSE)
  expect_s3_class(res_plot, "patchwork")
})
test_that("Returns a plot - mixed-effects population level weighted
          residuals", {
  data("exp_mixed_data")
  growth_model_summary <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "least-squares",
    function_type = "exponential")
  res_plot <- growth_model_residual_plots(growth_model_summary,
                                          residual_type = "population")
  expect_s3_class(res_plot, "patchwork")
})
test_that("Returns a plot - mixed-effects population level raw
          residuals", {
  data("exp_mixed_data")
  growth_model_summary <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "least-squares",
    function_type = "exponential")
  res_plot <- growth_model_residual_plots(growth_model_summary,
                                          residual_type = "population",
                                          weighted = FALSE)
  expect_s3_class(res_plot, "patchwork")
})
