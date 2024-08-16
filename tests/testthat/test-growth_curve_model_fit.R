test_that("Exponential model is returned and fixed effects are equal", {
  data("exp_mixed_data")
  exp_mixed_model <- exponential_mixed_model(
    data_frame = exp_mixed_data,
    model_type = "mixed"
  )
  growth_model <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "mixed",
    function_type = "exponential")

  summary_exp_func <- summary(exp_mixed_model)
  summary_growth_func <- summary(growth_model)

  expect_equal(summary_exp_func$coefficients$fixed, summary_growth_func$coefficients$fixed)
})
test_that("Linear model is returned and fixed effects are equal", {
  data("lin_mixed_data")
  lin_mixed_model <- linear_mixed_model(
    data_frame = lin_mixed_data,
    model_type = "mixed"
  )
  growth_model <- growth_curve_model_fit(
    data_frame = lin_mixed_data,
    model_type = "mixed",
    function_type = "linear")

  summary_lin_func <- summary(lin_mixed_model)
  summary_growth_func <- summary(growth_model)

  expect_equal(summary_lin_func$coefficients$fixed, summary_growth_func$coefficients$fixed)
})
test_that("Logistic model is returned and fixed effects are equal", {
  data("log_mixed_data")
  log_mixed_model <- logistic_mixed_model(
    data_frame = log_mixed_data,
    model_type = "mixed"
  )
  growth_model <- growth_curve_model_fit(
    data_frame = log_mixed_data,
    model_type = "mixed",
    function_type = "logistic")

  summary_log_func <- summary(log_mixed_model)
  summary_growth_func <- summary(growth_model)

  expect_equal(summary_log_func$coefficients$fixed, summary_growth_func$coefficients$fixed)
})
test_that("Gompertz model is returned and fixed effects are equal", {
  data("gomp_mixed_data")
  gomp_mixed_model <- gompertz_mixed_model(
    data_frame = gomp_mixed_data,
    model_type = "mixed"
  )
  growth_model <- growth_curve_model_fit(
    data_frame = gomp_mixed_data,
    model_type = "mixed",
    function_type = "gompertz")

  summary_gomp_func <- summary(gomp_mixed_model)
  summary_growth_func <- summary(growth_model)

  expect_equal(summary_gomp_func$coefficients$fixed, summary_growth_func$coefficients$fixed)
})

