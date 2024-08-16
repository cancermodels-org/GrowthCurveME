test_that("Returns a mixed-effects model", {
  data("gomp_mixed_data")
  gomp_mixed_model <- gompertz_mixed_model(data_frame = gomp_mixed_data,
                                             model_type = "mixed")
  expect_s3_class(gomp_mixed_model, "nlme")
})
test_that("Returns a least-squares model", {
  data("gomp_mixed_data")
  gomp_nls_model <- gompertz_mixed_model(data_frame = gomp_mixed_data,
                                           model_type = "least-squares")
  expect_s3_class(gomp_nls_model, "nls")
})
test_that("Returns a mixed-effects model - random slope", {
  data("gomp_mixed_data")
  gomp_mixed_model <- gompertz_mixed_model(data_frame = gomp_mixed_data,
                                             model_type = "mixed",
                                             fixed_rate = FALSE)
  expect_s3_class(gomp_mixed_model, "nlme")
})
test_that("Length of input data and model fitted data are the same", {
  data("gomp_mixed_data")
  gomp_mixed_model <- gompertz_mixed_model(data_frame = gomp_mixed_data,
                                             model_type = "mixed")
  expect_equal(nrow(gomp_mixed_data), length(gomp_mixed_model$fitted[,1]))
})
test_that("Fitted values fixed do NOT equal fitted values random", {
  data("gomp_mixed_data")
  gomp_mixed_model <- gompertz_mixed_model(data_frame = gomp_mixed_data,
                                             model_type = "mixed")
  fitted_fixed <- as.numeric(gomp_mixed_model$fitted[,1])
  fitted_random <- as.numeric(gomp_mixed_model$fitted[,2])
  expect_false(identical(fitted_fixed, fitted_random))
})
