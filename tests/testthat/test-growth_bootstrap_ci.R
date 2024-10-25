test_that("Returns a list of length four", {
  data("exp_mixed_data")
  exp_mixed_model <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "mixed",
    function_type = "exponential",
    return_summary = FALSE)
  growth_model_summary <- growth_curve_model_fit(
    data_frame = exp_mixed_data,
    model_type = "mixed",
    function_type = "exponential",
    return_summary = TRUE)
  growth_model_summary_append <- growth_bootstrap_ci(
    data_frame = exp_mixed_data,
    growth_model_object = exp_mixed_model,
    growth_model_summary_list = growth_model_summary,
    boot_n_sim = 5,
    mix_boot_method = "case"
  )

  expect_length(growth_model_summary_append, 4)
  expect_true(all(colnames(growth_model_summary_append[[4]]) %in%
                    c("time", "sim_pop_pred_value",
                      "sim_pop_pred_lb", "sim_pop_pred_ub")))
})
