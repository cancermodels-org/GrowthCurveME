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

  expect_s4_class(exp_mixed_model, "SaemixObject")
  expect_s4_class(growth_model, "SaemixObject")
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

  expect_s4_class(lin_mixed_model, "SaemixObject")
  expect_s4_class(growth_model, "SaemixObject")
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

  expect_s4_class(log_mixed_model, "SaemixObject")
  expect_s4_class(growth_model, "SaemixObject")
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

  expect_s4_class(gomp_mixed_model, "SaemixObject")
  expect_s4_class(growth_model, "SaemixObject")
})

