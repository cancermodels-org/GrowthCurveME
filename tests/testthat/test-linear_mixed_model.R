test_that("Returns a mixed-effects model", {
  data("lin_mixed_data")
  lin_mixed_model <-linear_mixed_model(
    data_frame = lin_mixed_data,
    model_type = "mixed",
  )
  expect_true(any(c("SaemixObject", "saemix") %in% class(lin_mixed_model)))
})
test_that("Returns a least-squares model", {
  data("lin_mixed_data")
  lin_nls_model <-linear_mixed_model(
    data_frame = lin_mixed_data,
    model_type = "least-squares"
  )
  expect_s3_class(lin_nls_model, "nls")
})
test_that("Returns a mixed-effects model - random slope", {
  data("lin_mixed_data")
  lin_mixed_model <-linear_mixed_model(
    data_frame = lin_mixed_data,
    model_type = "mixed",
    fixed_rate = FALSE
  )
  expect_true(any(c("SaemixObject", "saemix") %in% class(lin_mixed_model)))
})
