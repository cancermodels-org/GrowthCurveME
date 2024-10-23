test_that("Returns a mixed-effects model", {
  data("gomp_mixed_data")
  gomp_mixed_model <-gompertz_mixed_model(
    data_frame = gomp_mixed_data,
    model_type = "mixed",
  )
  expect_true(any(c("SaemixObject", "saemix") %in% class(gomp_mixed_model)))
})
test_that("Returns a least-squares model", {
  data("gomp_mixed_data")
  gomp_nls_model <-gompertz_mixed_model(
    data_frame = gomp_mixed_data,
    model_type = "least-squares"
  )
  expect_s3_class(gomp_nls_model, "nls")
})
test_that("Returns a mixed-effects model - random slope", {
  data("gomp_mixed_data")
  gomp_mixed_model <-gompertz_mixed_model(
    data_frame = gomp_mixed_data,
    model_type = "mixed",
    fixed_rate = FALSE
  )
  expect_true(any(c("SaemixObject", "saemix") %in% class(gomp_mixed_model)))
})
