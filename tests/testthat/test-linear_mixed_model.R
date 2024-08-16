test_that("Returns a mixed-effects model", {
  data("lin_mixed_data")
  lin_mixed_model <- linear_mixed_model(data_frame = lin_mixed_data,
                                           model_type = "mixed")
  expect_s3_class(lin_mixed_model, "lme")
})
test_that("Returns a least-squares model", {
  data("lin_mixed_data")
  lin_nls_model <- linear_mixed_model(data_frame = lin_mixed_data,
                                         model_type = "least-squares")
  expect_s3_class(lin_nls_model, "lm")
})
test_that("Returns a mixed-effects model - random slope", {
  data("lin_mixed_data")
  lin_mixed_model <- linear_mixed_model(data_frame = lin_mixed_data,
                                           model_type = "mixed",
                                           fixed_rate = FALSE)
  expect_s3_class(lin_mixed_model, "lme")
})
test_that("Length of input data and model fitted data are the same", {
  data("lin_mixed_data")
  lin_mixed_model <- linear_mixed_model(data_frame = lin_mixed_data,
                                           model_type = "mixed")
  expect_equal(nrow(lin_mixed_data), length(lin_mixed_model$fitted[,1]))
})
test_that("Fitted values fixed do NOT equal fitted values random", {
  data("lin_mixed_data")
  lin_mixed_model <- linear_mixed_model(data_frame = lin_mixed_data,
                                           model_type = "mixed")
  fitted_fixed <- as.numeric(lin_mixed_model$fitted[,1])
  fitted_random <- as.numeric(lin_mixed_model$fitted[,2])
  expect_false(identical(fitted_fixed, fitted_random))
})
