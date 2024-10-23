test_that("Returns a mixed-effects model", {
  data("exp_mixed_data")
  exp_mixed_model <- exponential_mixed_model(
    data_frame = exp_mixed_data,
    model_type = "mixed",
  )
  expect_true(any(c("SaemixObject", "saemix") %in% class(exp_mixed_model)))
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
  expect_true(any(c("SaemixObject", "saemix") %in% class(exp_mixed_model)))
})
