test_that("Returns a mixed-effects model", {
  data("exp_mixed_data")
  exp_mixed_model <- exponential_mixed_model(
    data_frame = exp_mixed_data,
    model_type = "mixed"
  )
  expect_s3_class(exp_mixed_model, "nlme")
})
test_that("Returns a least-squares model", {
  data("exp_mixed_data")
  exp_nls_model <- exponential_mixed_model(
    data_frame = exp_mixed_data,
    model_type = "least-squares"
  )
  expect_s3_class(exp_nls_model, "nls")
})
test_that("Returns a mixed-effects model - random slope", {
  data("exp_mixed_data")
  exp_mixed_model <- exponential_mixed_model(
    data_frame = exp_mixed_data,
    model_type = "mixed",
    fixed_rate = FALSE
  )
  expect_s3_class(exp_mixed_model, "nlme")
})
test_that("Length of input data and model fitted data are the same", {
  data("exp_mixed_data")
  exp_mixed_model <- exponential_mixed_model(
    data_frame = exp_mixed_data,
    model_type = "mixed"
  )
  expect_equal(nrow(exp_mixed_data), length(exp_mixed_model$fitted[, 1]))
})
test_that("Fitted values fixed do NOT equal fitted values random", {
  data("exp_mixed_data")
  exp_mixed_model <- exponential_mixed_model(
    data_frame = exp_mixed_data,
    model_type = "mixed"
  )
  fitted_fixed <- as.numeric(exp_mixed_model$fitted[, 1])
  fitted_random <- as.numeric(exp_mixed_model$fitted[, 2])
  expect_false(identical(fitted_fixed, fitted_random))
})
