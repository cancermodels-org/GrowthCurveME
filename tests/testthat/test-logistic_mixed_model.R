test_that("Returns a mixed-effects model", {
  data("log_mixed_data")
  log_mixed_model <- logistic_mixed_model(data_frame = log_mixed_data,
                                        model_type = "mixed")
  expect_s3_class(log_mixed_model, "nlme")
})
test_that("Returns a least-squares model", {
  data("log_mixed_data")
  log_nls_model <- logistic_mixed_model(data_frame = log_mixed_data,
                                      model_type = "least-squares")
  expect_s3_class(log_nls_model, "nls")
})
test_that("Returns a mixed-effects model - random slope", {
  data("log_mixed_data")
  log_mixed_model <- logistic_mixed_model(data_frame = log_mixed_data,
                                        model_type = "mixed",
                                        fixed_rate = FALSE)
  expect_s3_class(log_mixed_model, "nlme")
})
test_that("Length of input data and model fitted data are the same", {
  data("log_mixed_data")
  log_mixed_model <- logistic_mixed_model(data_frame = log_mixed_data,
                                        model_type = "mixed")
  expect_equal(nrow(log_mixed_data), length(log_mixed_model$fitted[,1]))
})
test_that("Fitted values fixed do NOT equal fitted values random", {
  data("log_mixed_data")
  log_mixed_model <- logistic_mixed_model(data_frame = log_mixed_data,
                                        model_type = "mixed")
  fitted_fixed <- as.numeric(log_mixed_model$fitted[,1])
  fitted_random <- as.numeric(log_mixed_model$fitted[,2])
  expect_false(identical(fitted_fixed, fitted_random))
})

