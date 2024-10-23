test_that("Returns a mixed-effects model", {
  data("log_mixed_data")
  log_mixed_model <-logistic_mixed_model(
    data_frame = log_mixed_data,
    model_type = "mixed",
  )
  expect_true(any(c("SaemixObject", "saemix") %in% class(log_mixed_model)))
})
test_that("Returns a least-squares model", {
  data("log_mixed_data")
  log_nls_model <-logistic_mixed_model(
    data_frame = log_mixed_data,
    model_type = "least-squares"
  )
  expect_s3_class(log_nls_model, "nls")
})
test_that("Returns a mixed-effects model - random slope", {
  data("log_mixed_data")
  log_mixed_model <-logistic_mixed_model(
    data_frame = log_mixed_data,
    model_type = "mixed",
    fixed_rate = FALSE
  )
  expect_true(any(c("SaemixObject", "saemix") %in% class(log_mixed_model)))
})

