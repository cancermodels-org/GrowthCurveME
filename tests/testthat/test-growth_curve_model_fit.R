test_that("Exponential model is returned and equal to direct output of
          model function", {
  data("exp_mixed_data")
  exp_mixed_model <- exponential_mixed_model(
    data_frame = exp_mixed_data,
    model_type = "mixed",
    seed = 123
  )
  growth_model <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "mixed",
    function_type = "exponential",
    return_summary = FALSE,
    seed = 123)

  expect_equal(exp_mixed_model, growth_model)
})
test_that("Linear model is returned and equal to direct output of
          model function", {
  data("lin_mixed_data")
  lin_mixed_model <- linear_mixed_model(
    data_frame = lin_mixed_data,
    model_type = "mixed",
    seed = 123
  )
  growth_model <- growth_curve_model_fit(
    data_frame = lin_mixed_data,
    model_type = "mixed",
    function_type = "linear",
    return_summary = FALSE,
    seed = 123)

  expect_equal(lin_mixed_model, growth_model)
})
test_that("Logistic model is returned and equal to direct output of
          model function", {
  data("log_mixed_data")
  log_mixed_model <- logistic_mixed_model(
    data_frame = log_mixed_data,
    model_type = "mixed",
    seed = 123
  )
  growth_model <- growth_curve_model_fit(
    data_frame = log_mixed_data,
    model_type = "mixed",
    function_type = "logistic",
    return_summary = FALSE,
    seed = 123)

  expect_equal(log_mixed_model, growth_model)
})
test_that("Gompertz model is returned and equal to direct output of
          model function", {
  data("gomp_mixed_data")
  gomp_mixed_model <- gompertz_mixed_model(
    data_frame = gomp_mixed_data,
    model_type = "mixed",
    seed = 123
  )
  growth_model <- growth_curve_model_fit(
    data_frame = gomp_mixed_data,
    model_type = "mixed",
    function_type = "gompertz",
    return_summary = FALSE,
    seed = 123)

  expect_equal(gomp_mixed_model, growth_model)
})
test_that("Boostrap feature returns list with 4 dataframes", {
  data("exp_mixed_data")
  growth_model_summary <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "mixed",
    function_type = "exponential",
    return_summary = TRUE,
    seed = 123)

  expect_length(growth_model_summary, 4)
})

