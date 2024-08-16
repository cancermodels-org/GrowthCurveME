#' Summarize least-squares growth model object and data
#'
#' @description
#' 'summarize_growth_model_mixed()' is a function used within the \code{\link{summarize_growth_model}} function to create a list object of data frames based on a user's input data frame and outputed least-squares growth model object from \code{\link{growth_curve_model_fit}}.
#' The list object (referred to in this package as 'growth_model_summary_list') can be used to extract model predicted values, residuals, and can be in-putted into supporting functions from GrowthCurveME to generate plots and perform model diagnostics.
#'
#' @inheritParams growth_curve_model_fit
#' @param ls_model The least-squares model object that is created using the 'growth_curve_model_fit()'
#'
#' @inherit summarize_growth_model return
#' @seealso \code{\link{growth_curve_model_fit}} \code{\link{summarize_growth_model}}
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange case_when filter mutate mutate_if rename select
#' @importFrom tibble rownames_to_column tibble
#' @importFrom stats AIC BIC confint logLik predict qqnorm residuals
#' @export
#'
#' @examples
#' # Load example data (exponential data)
#' data(exp_mixed_data)
#' # Fit an least-squares growth model to the data
#' exp_ls_model <- growth_curve_model_fit(data_frame = exp_mixed_data,
#' function_type = "exponential",
#' model_type = "least-squares")
#' # Summarize the data by creating a summary list object with main 'summarize_growth_model()' function
#' exp_ls_model_summary <- summarize_growth_model(data_frame = exp_mixed_data,
#' growth_model_object = exp_ls_model,
#' model_type = "least-squares",
#' function_type = "exponential")
#' # Summarize the data by creating a summary list object with main 'summarize_growth_model()' function
#' exp_ls_model_summary <- summarize_growth_model_ls(data_frame = exp_mixed_data,
#' ls_model = exp_ls_model,
#' function_type = "exponential")
summarize_growth_model_ls <- function(data_frame,
                                      ls_model,
                                      function_type = "exponential") {
  # Check initial inputs
  stopifnot(
    "cluster" %in% colnames(data_frame),
    "time" %in% colnames(data_frame),
    "growth_metric" %in% colnames(data_frame),
    is.numeric(data_frame$growth_metric),
    is.numeric(data_frame$time),
    function_type %in% c(
      "exponential", "linear", "logistic",
      "gompertz"
    ),
    inherits(ls_model, c("lm", "nls"))
  )

  # Remove missing values from cluster, time, and growth_metric variables
  data_frame <- data_frame %>%
    dplyr::filter(
      !is.na(cluster),
      !is.na(time),
      !is.na(growth_metric)
    )

  # Arrange data_frame by time and cluster
  data_frame <- data_frame %>%
    dplyr::arrange(time, cluster)


  # Create summary object
  sum_object <- summary(ls_model)
  # Compute confidence intervals
  ci_intervals <- data.frame(confint(ls_model))
  # If regression ls_model function_type is exponential
  if (function_type == "exponential") {
    # Create wide data set to store variables and components within it
    model_summary_wide <- tibble::tibble(
      model_function = function_type,
      model_type = "least-squares",
      number_observations = data_frame %>%
        filter(!is.na(time), !is.na(growth_metric)) %>%
        nrow(),
      intercept = sum_object$coefficients[1, 1],
      intercept_lb = ci_intervals[1, 1],
      intercept_ub = ci_intervals[1, 2],
      intercept_t_statistic = sum_object$coefficients[1, 3],
      intercept_p_value = sum_object$coefficients[1, 4],
      rate = sum_object$coefficients[2, 1],
      rate_lb = ci_intervals[2, 1],
      rate_ub = ci_intervals[2, 2],
      rate_t_statistic = sum_object$coefficients[2, 3],
      rate_p_value = sum_object$coefficients[2, 4],
      double_time_hours = log(2) / rate,
      double_time_hours_lb = log(2) / rate_ub,
      double_time_hours_ub = log(2) / rate_lb,
      AIC = stats::AIC(ls_model),
      BIC = stats::BIC(ls_model),
      loglik = as.numeric(stats::logLik(ls_model))
    )

    # Round and create tidy variables for convert to long dataset for table figure
    model_summary_long <- model_summary_wide %>%
      dplyr::mutate(
        intercept = round(intercept, 2),
        intercept_lb = round(intercept_lb, 2),
        intercept_ub = round(intercept_ub, 2),
        intercept_ci = paste(intercept, " [", intercept_lb, ",", intercept_ub, "]",
          sep = ""
        ),
        intercept_t_statistic = round(intercept_t_statistic, 2),
        intercept_p_value = dplyr::case_when(
          intercept_p_value >= 0.0001 ~
            as.character(round(intercept_p_value, 4)),
          TRUE ~ "<0.0001"
        ),
        rate = round(rate, 6),
        rate_lb = round(rate_lb, 6),
        rate_ub = round(rate_ub, 6),
        rate_ci = paste(rate, " [", rate_lb, ",", rate_ub, "]", sep = ""),
        rate_t_statistic = round(rate_t_statistic, 2),
        rate_p_value = dplyr::case_when(
          rate_p_value >= 0.0001 ~
            as.character(round(rate_p_value, 4)),
          TRUE ~ "<0.0001"
        ),
        double_time_hours = round(double_time_hours, 2),
        double_time_hours_lb = round(double_time_hours_lb, 2),
        double_time_hours_ub = round(double_time_hours_ub, 2),
        double_time_ci = paste(double_time_hours, " [", double_time_hours_lb, ",",
          double_time_hours_ub, "]",
          sep = ""
        ),
        AIC = round(AIC, 2),
        BIC = round(BIC, 2),
        loglik = round(loglik, 2)
      ) %>%
      dplyr::mutate_if(is.numeric, as.character) %>%
      dplyr::select(
        "Model function" = model_function,
        "Model type" = model_type,
        "Number of observations" = number_observations,
        "Intercept [95% CI]" = intercept_ci,
        "Intercept p-value" = intercept_p_value,
        "Rate constant [95% CI]" = rate_ci,
        "Rate constant p-value" = rate_p_value,
        "Doubling time estimate [95% CI]" = double_time_ci,
        "Akaike information criterion (AIC)" = AIC,
        "Bayesian information criterion (BIC)" = BIC,
        "Log likelihood" = loglik
      )
    # Transpose and convert to a dataframe
    model_summary_long <- as.data.frame(t(model_summary_long))
    # Convert rownames to a column and rename to Value
    model_summary_long <- model_summary_long %>%
      tibble::rownames_to_column(var = "Variable") %>%
      dplyr::rename("Value" = "V1")
  }

  # If regression ls_model function_type is linear
  if (function_type == "linear") {
    # Create wide data set to store variables and components within it
    model_summary_wide <- tibble::tibble(
      model_function = function_type,
      model_type = "least-squares",
      number_observations = data_frame %>%
        filter(!is.na(time), !is.na(growth_metric)) %>%
        nrow(),
      intercept = sum_object$coefficients[1, 1],
      intercept_lb = ci_intervals[1, 1],
      intercept_ub = ci_intervals[1, 2],
      intercept_t_statistic = sum_object$coefficients[1, 3],
      intercept_p_value = sum_object$coefficients[1, 4],
      rate = sum_object$coefficients[2, 1],
      rate_lb = ci_intervals[2, 1],
      rate_ub = ci_intervals[2, 2],
      rate_t_statistic = sum_object$coefficients[2, 3],
      rate_p_value = sum_object$coefficients[2, 4],
      double_time_hours = 2 / rate,
      double_time_hours_lb = 2 / rate_ub,
      double_time_hours_ub = 2 / rate_lb,
      AIC = stats::AIC(ls_model),
      BIC = stats::BIC(ls_model),
      loglik = as.numeric(stats::logLik(ls_model))
    )

    # Round and create tidy variables for convert to long dataset for table figure
    model_summary_long <- model_summary_wide %>%
      dplyr::mutate(
        intercept = round(intercept, 2),
        intercept_lb = round(intercept_lb, 2),
        intercept_ub = round(intercept_ub, 2),
        intercept_ci = paste(intercept, " [", intercept_lb, ",", intercept_ub, "]",
          sep = ""
        ),
        intercept_t_statistic = round(intercept_t_statistic, 2),
        intercept_p_value = dplyr::case_when(
          intercept_p_value >= 0.0001 ~
            as.character(round(intercept_p_value, 4)),
          TRUE ~ "<0.0001"
        ),
        rate = round(rate, 6),
        rate_lb = round(rate_lb, 6),
        rate_ub = round(rate_ub, 6),
        rate_ci = paste(rate, " [", rate_lb, ",", rate_ub, "]", sep = ""),
        rate_t_statistic = round(rate_t_statistic, 2),
        rate_p_value = dplyr::case_when(
          rate_p_value >= 0.0001 ~
            as.character(round(rate_p_value, 4)),
          TRUE ~ "<0.0001"
        ),
        double_time_hours = round(double_time_hours, 2),
        double_time_hours_lb = round(double_time_hours_lb, 2),
        double_time_hours_ub = round(double_time_hours_ub, 2),
        double_time_ci = paste(double_time_hours, " [", double_time_hours_lb, ",",
          double_time_hours_ub, "]",
          sep = ""
        ),
        AIC = round(AIC, 2),
        BIC = round(BIC, 2),
        loglik = round(loglik, 2)
      ) %>%
      dplyr::mutate_if(is.numeric, as.character) %>%
      dplyr::select(
        "Model function" = model_function,
        "Model type" = model_type,
        "Number of observations" = number_observations,
        "Intercept [95% CI]" = intercept_ci,
        "Intercept p-value" = intercept_p_value,
        "Rate constant [95% CI]" = rate_ci,
        "Rate constant p-value" = rate_p_value,
        "Doubling time estimate [95% CI]" = double_time_ci,
        "Akaike information criterion (AIC)" = AIC,
        "Bayesian information criterion (BIC)" = BIC,
        "Log likelihood" = loglik
      )
    # Transpose and convert to a dataframe
    model_summary_long <- as.data.frame(t(model_summary_long))
    # Convert rownames to a column and rename to Value
    model_summary_long <- model_summary_long %>%
      tibble::rownames_to_column(var = "Variable") %>%
      dplyr::rename("Value" = "V1")
  }

  # If ls_model function_type is logistic or Gompertz
  if (function_type == "logistic" | function_type == "gompertz") {
    # Create wide data set to store variables and components within it
    model_summary_wide <- tibble::tibble(
      model_function = function_type,
      model_type = "least-squares",
      number_observations = data_frame %>%
        filter(!is.na(time), !is.na(growth_metric)) %>%
        nrow(),
      lower_asy = sum_object$coefficients[1, 1],
      lower_asy_lb = ci_intervals[1, 1],
      lower_asy_ub = ci_intervals[1, 2],
      lower_asy_t_statistic = sum_object$coefficients[1, 3],
      lower_asy_p_value = sum_object$coefficients[1, 4],
      upper_asy = sum_object$coefficients[2, 1],
      upper_asy_lb = ci_intervals[2, 1],
      upper_asy_ub = ci_intervals[2, 2],
      upper_asy_t_statistic = sum_object$coefficients[2, 3],
      upper_asy_p_value = sum_object$coefficients[2, 4],
      rate = sum_object$coefficients[3, 1],
      rate_lb = ci_intervals[3, 1],
      rate_ub = ci_intervals[3, 2],
      rate_t_statistic = sum_object$coefficients[3, 3],
      rate_p_value = sum_object$coefficients[3, 4],
      inflection = sum_object$coefficients[4, 1],
      inflection_lb = ci_intervals[4, 1],
      inflection_ub = ci_intervals[4, 2],
      inflection_t_statistic = sum_object$coefficients[4, 3],
      inflection_p_value = sum_object$coefficients[4, 4],
      double_time_hours = log(2) / rate,
      double_time_hours_lb = log(2) / rate_ub,
      double_time_hours_ub = log(2) / rate_lb,
      AIC = stats::AIC(ls_model),
      BIC = stats::BIC(ls_model),
      loglik = as.numeric(stats::logLik(ls_model))
    )

    # Round and create tidy variables for convert to long dataset for table figure
    model_summary_long <- model_summary_wide %>%
      dplyr::mutate(
        lower_asy = round(lower_asy, 2),
        lower_asy_lb = round(lower_asy_lb, 2),
        lower_asy_ub = round(lower_asy_ub, 2),
        lower_asy_ci = paste(lower_asy, " [", lower_asy_lb, ",", lower_asy_ub, "]",
          sep = ""
        ),
        lower_asy_t_statistic = round(lower_asy_t_statistic, 2),
        lower_asy_p_value = dplyr::case_when(
          rate_p_value >= 0.0001 ~
            as.character(round(rate_p_value, 4)),
          TRUE ~ "<0.0001"
        ),
        upper_asy = round(upper_asy, 2),
        upper_asy_lb = round(upper_asy_lb, 2),
        upper_asy_ub = round(upper_asy_ub, 2),
        upper_asy_ci = paste(upper_asy, " [", upper_asy_lb, ",", upper_asy_ub, "]",
          sep = ""
        ),
        upper_asy_t_statistic = round(upper_asy_t_statistic, 2),
        upper_asy_p_value = dplyr::case_when(
          rate_p_value >= 0.0001 ~
            as.character(round(rate_p_value, 4)),
          TRUE ~ "<0.0001"
        ),
        inflection = round(inflection, 2),
        inflection_lb = round(inflection_lb, 2),
        inflection_ub = round(inflection_ub, 2),
        inflection_ci = paste(inflection, " [", inflection_lb, ",", inflection_ub, "]",
          sep = ""
        ),
        inflection_t_statistic = round(inflection_t_statistic, 2),
        inflection_p_value = dplyr::case_when(
          rate_p_value >= 0.0001 ~
            as.character(round(rate_p_value, 4)),
          TRUE ~ "<0.0001"
        ),
        rate = round(rate, 6),
        rate_lb = round(rate_lb, 6),
        rate_ub = round(rate_ub, 6),
        rate_ci = paste(rate, " [", rate_lb, ",", rate_ub, "]", sep = ""),
        rate_t_statistic = round(rate_t_statistic, 2),
        rate_p_value = dplyr::case_when(
          rate_p_value >= 0.0001 ~
            as.character(round(rate_p_value, 4)),
          TRUE ~ "<0.0001"
        ),
        double_time_hours = round(double_time_hours, 2),
        double_time_hours_lb = round(double_time_hours_lb, 2),
        double_time_hours_ub = round(double_time_hours_ub, 2),
        double_time_ci = paste(double_time_hours, " [", double_time_hours_lb, ",",
          double_time_hours_ub, "]",
          sep = ""
        ),
        AIC = round(AIC, 2),
        BIC = round(BIC, 2),
        loglik = round(loglik, 2)
      ) %>%
      dplyr::mutate_if(is.numeric, as.character) %>%
      dplyr::select(
        "Model function" = model_function,
        "Model type" = model_type,
        "Number of observations" = number_observations,
        "Lower asymptote estimate [95% CI]" = lower_asy_ci,
        "Lower asymptote p-value" = lower_asy_p_value,
        "Upper asymptote estimate [95% CI]" = upper_asy_ci,
        "Upper asymptote p-value" = upper_asy_p_value,
        "Inflection point estimate [95% CI]" = inflection_ci,
        "Inflection point p-value" = inflection_p_value,
        "Rate constant estimate [95% CI]" = rate_ci,
        "Rate constant p-value" = rate_p_value,
        "Doubling time estimate [95% CI]" = double_time_ci,
        "Akaike information criterion (AIC)" = AIC,
        "Bayesian information criterion (BIC)" = BIC
      )
    # Transpose data and convert to dataframe
    model_summary_long <- as.data.frame(t(model_summary_long))
    # Move rownames to a column and rename to Value
    model_summary_long <- model_summary_long %>%
      tibble::rownames_to_column(var = "Variable") %>%
      dplyr::rename("Value" = "V1")
  }

  # Extract original data used to fit ls_model
  model_residual_data <- data_frame
  # Extract residuals and create empty variable columns
  model_residual_data <- model_residual_data %>%
    dplyr::mutate(
      fitted_values_fixed = predict(ls_model),
      marginal_raw_residuals = stats::residuals(ls_model),
      marginal_standardized_residuals = stats::residuals(ls_model, function_type = "pearson"),
      marginal_standardized_theoretical_quantiles =
        stats::qqnorm(marginal_standardized_residuals, plot.it = FALSE)$x
    )
  # Create a list of wide and long ls_model summary datasets
  model_summary_list <- list(
    "model_summary_wide" = model_summary_wide,
    "model_summary_long" = model_summary_long,
    "model_residual_data" = model_residual_data
  )

  # Return the model_summary_list
  return(model_summary_list)
}
