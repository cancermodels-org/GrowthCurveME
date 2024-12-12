#' Summarize least-squares growth model object and data
#'
#' @description
#' This function is used within the \code{\link{summarize_growth_model}}
#' function to create a list object of data frames based on a user's input
#' data frame and output least-squares growth model object
#' from \code{\link{growth_curve_model_fit}}.
#' The list object (referred to in this package as 'growth_model_summary_list')
#' can be used to extract model predicted values, residuals,
#' and can be inputted into supporting functions from GrowthCurveME to
#' generate plots and perform model diagnostics.
#'
#' @inheritParams growth_curve_model_fit
#' @param ls_model The least-squares model object that is created using
#' the \code{\link{growth_curve_model_fit}}
#'
#' @inherit summarize_growth_model return
#' @seealso
#' \code{\link{growth_curve_model_fit}}
#' \code{\link{summarize_growth_model}}
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange bind_cols case_when distinct filter group_by
#' mutate mutate_if rename select summarize
#' @importFrom tibble rownames_to_column tibble
#' @importFrom stats AIC BIC confint logLik predict quantile qqnorm residuals
#' @importFrom rlang sym :=
#' @importFrom investr predFit
#' @export
#'
#' @examples
#' # Load example data (exponential data)
#' data(exp_mixed_data)
#' # Fit an mixed-effects growth model to the data
#' exp_ls_model <- growth_curve_model_fit(
#' data_frame = exp_mixed_data,
#' function_type = "exponential",
#' model_type = "least-squares",
#' return_summary = FALSE)
#' # Summarize the data by creating a summary list object
#' exp_ls_model_summary <- summarize_growth_model_ls(
#' data_frame = exp_mixed_data,
#' ls_model = exp_ls_model,
#' function_type = "exponential",
#' time_unit = "hours")
summarize_growth_model_ls <- function(data_frame,
                                      ls_model,
                                      function_type = "exponential",
                                      time_unit = "hours") {
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
    inherits(ls_model, "nls")
  )

  # Remove missing values from cluster, time, and growth_metric variables
  data_frame <- data_frame %>%
    dplyr::filter(
      !is.na(!!rlang::sym("cluster")),
      !is.na(!!rlang::sym("time")),
      !is.na(!!rlang::sym("growth_metric"))
    )

  # Arrange data_frame by time and cluster
  data_frame <- data_frame %>%
    dplyr::arrange(!!rlang::sym("cluster"), !!rlang::sym("time"))


  # Create summary object
  sum_object <- summary(ls_model)
  # Compute confidence intervals
  ci_intervals <- suppressMessages(
    data.frame(stats::confint(ls_model))
  )
  # If regression ls_model function_type is exponential
  if (function_type %in% c("exponential", "linear")) {
    # Create wide data set to store variables and components within it
    model_summary_wide <- tibble::tibble(
      model_function = function_type,
      model_type = "least-squares",
      number_observations = data_frame %>%
        nrow() %>% as.numeric(),
      intercept_est = sum_object$coefficients[1, 1],
      intercept_se = sum_object$coefficients[1,2],
      intercept_lb = ci_intervals[1, 1],
      intercept_ub = ci_intervals[1, 2],
      rate_est = sum_object$coefficients[2, 1],
      rate_se = sum_object$coefficients[2,2],
      rate_lb = ci_intervals[2, 1],
      rate_ub = ci_intervals[2, 2],
    )

    # Calculate doubling time and add aic, bic, and loglik
    if (function_type == "linear") {
      model_summary_wide <- model_summary_wide %>%
        dplyr::mutate(
          double_time_est =
            !!rlang::sym("intercept_est") / !!rlang::sym("rate_est"),
          double_time_lb =
            !!rlang::sym("intercept_est") / !!rlang::sym("rate_ub"),
          double_time_ub =
            !!rlang::sym("intercept_est") / !!rlang::sym("rate_lb"),
          aic = stats::AIC(ls_model),
          bic = stats::BIC(ls_model),
          loglik = as.numeric(stats::logLik(ls_model))
        )
    } else {
      model_summary_wide <- model_summary_wide %>%
        dplyr::mutate(
          double_time_est = log(2) / !!rlang::sym("rate_est"),
          double_time_lb = log(2) / !!rlang::sym("rate_ub"),
          double_time_ub = log(2) / !!rlang::sym("rate_lb"),
          aic = stats::AIC(ls_model),
          bic = stats::BIC(ls_model),
          loglik = as.numeric(stats::logLik(ls_model))
        )
    }

    # Round and create tidy variables for convert to long dataset for
    # table figure
    model_summary_long <- model_summary_wide %>%
      dplyr::mutate(
        intercept_est = round(!!rlang::sym("intercept_est"), 2),
        intercept_lb = round(!!rlang::sym("intercept_lb"), 2),
        intercept_ub = round(!!rlang::sym("intercept_ub"), 2),
        intercept_ci = paste(
          !!rlang::sym("intercept_est"),
          " [",
          !!rlang::sym("intercept_lb"),
          ",",
          !!rlang::sym("intercept_ub"),
          "]",
          sep = ""
        ),
        rate_est = round(!!rlang::sym("rate_est"), 6),
        rate_lb = round(!!rlang::sym("rate_lb"), 6),
        rate_ub = round(!!rlang::sym("rate_ub"), 6),
        rate_ci = paste(
          !!rlang::sym("rate_est"),
          " [",
          !!rlang::sym("rate_lb"),
          ",",
          !!rlang::sym("rate_ub"),
          "]",
          sep = ""
        ),
        double_time_est = round(!!rlang::sym("double_time_est"), 2),
        double_time_lb = round(!!rlang::sym("double_time_lb"), 2),
        double_time_ub = round(!!rlang::sym("double_time_ub"), 2),
        double_time_ci = paste(
          !!rlang::sym("double_time_est"),
          " [",
          !!rlang::sym("double_time_lb"),
          ",",
          !!rlang::sym("double_time_ub"),
          "]",
          sep = ""
        ),
        aic = round(!!rlang::sym("aic"), 2),
        bic = round(!!rlang::sym("bic"), 2),
        loglik = round(!!rlang::sym("loglik"), 2)
      ) %>%
      dplyr::mutate_if(is.numeric, as.character) %>%
      dplyr::select(
        "Growth function" = !!rlang::sym("model_function"),
        "Regression type" = !!rlang::sym("model_type"),
        "Number of observations" = !!rlang::sym("number_observations"),
        "Intercept [95% CI]" = !!rlang::sym("intercept_ci"),
        "Rate constant [95% CI]" = !!rlang::sym("rate_ci"),
        !!paste("Doubling time estimate (",
                time_unit,
                ") [95% CI]",
                sep = "") := !!rlang::sym("double_time_ci"),
        "Akaike information criterion (AIC)" = !!rlang::sym("aic"),
        "Bayesian information criterion (BIC)" = !!rlang::sym("bic"),
        "Log likelihood" = !!rlang::sym("loglik")
      )
    # Transpose and convert to a dataframe
    model_summary_long <- as.data.frame(t(model_summary_long))

    # Convert rownames to a column and rename to Value
    model_summary_long <- model_summary_long %>%
      tibble::rownames_to_column(var = "Variable") %>%
      dplyr::rename("Value" = !!rlang::sym("V1"))

  }

  # If ls_model function_type is logistic or Gompertz
  if (function_type == "logistic" | function_type == "gompertz") {
    # Create wide data set to store variables and components within it
    model_summary_wide <- tibble::tibble(
      model_function = function_type,
      model_type = "least-squares",
      number_observations = data_frame %>%
        nrow() %>% as.numeric(),
      lower_asy_est = sum_object$coefficients[1, 1],
      lower_asy_se = sum_object$coefficients[1, 2],
      lower_asy_lb = ci_intervals[1, 1],
      lower_asy_ub = ci_intervals[1, 2],
      upper_asy_est = sum_object$coefficients[2, 1],
      upper_asy_se = sum_object$coefficients[2, 2],
      upper_asy_lb = ci_intervals[2, 1],
      upper_asy_ub = ci_intervals[2, 2],
      rate_est = sum_object$coefficients[3, 1],
      rate_se = sum_object$coefficients[3, 2],
      rate_lb = ci_intervals[3, 1],
      rate_ub = ci_intervals[3, 2],
      inflection_est = sum_object$coefficients[4, 1],
      inflection_se = sum_object$coefficients[4, 2],
      inflection_lb = ci_intervals[4, 1],
      inflection_ub = ci_intervals[4, 2],
      double_time_est = log(2) / !!rlang::sym("rate_est"),
      double_time_lb = log(2) / !!rlang::sym("rate_ub"),
      double_time_ub = log(2) / !!rlang::sym("rate_lb"),
      aic = stats::AIC(ls_model),
      bic = stats::BIC(ls_model),
      loglik = as.numeric(stats::logLik(ls_model))
    )

    # Round and create tidy variables for convert to long dataset for table
    # figure
    model_summary_long <- model_summary_wide %>%
      dplyr::mutate(
        lower_asy_est = round(!!rlang::sym("lower_asy_est"), 2),
        lower_asy_lb = round(!!rlang::sym("lower_asy_lb"), 2),
        lower_asy_ub = round(!!rlang::sym("lower_asy_ub"), 2),
        lower_asy_ci = paste(
          !!rlang::sym("lower_asy_est"),
          " [",
          !!rlang::sym("lower_asy_lb"),
          ",",
          !!rlang::sym("lower_asy_ub"),
          "]",
          sep = ""
        ),
        upper_asy_est = round(!!rlang::sym("upper_asy_est"), 2),
        upper_asy_lb = round(!!rlang::sym("upper_asy_lb"), 2),
        upper_asy_ub = round(!!rlang::sym("upper_asy_ub"), 2),
        upper_asy_ci = paste(
          !!rlang::sym("upper_asy_est"),
          " [",
          !!rlang::sym("upper_asy_lb"),
          ",",
          !!rlang::sym("upper_asy_ub"),
          "]",
          sep = ""
        ),
        inflection_est = round(!!rlang::sym("inflection_est"), 2),
        inflection_lb = round(!!rlang::sym("inflection_lb"), 2),
        inflection_ub = round(!!rlang::sym("inflection_ub"), 2),
        inflection_ci = paste(
          !!rlang::sym("inflection_est"),
          " [",
          !!rlang::sym("inflection_lb"),
          ",",
          !!rlang::sym("inflection_ub"),
          "]",
          sep = ""
        ),
        rate_est = round(!!rlang::sym("rate_est"), 6),
        rate_lb = round(!!rlang::sym("rate_lb"), 6),
        rate_ub = round(!!rlang::sym("rate_ub"), 6),
        rate_ci = paste(
          !!rlang::sym("rate_est"),
          " [",
          !!rlang::sym("rate_lb"),
          ",",
          !!rlang::sym("rate_ub"),
          "]",
          sep = ""
        ),
        double_time_est = round(!!rlang::sym("double_time_est"), 2),
        double_time_lb = round(!!rlang::sym("double_time_lb"), 2),
        double_time_ub = round(!!rlang::sym("double_time_ub"), 2),
        double_time_ci = paste(
          !!rlang::sym("double_time_est"),
          " [",
          !!rlang::sym("double_time_lb"),
          ",",
          !!rlang::sym("double_time_ub"),
          "]",
          sep = ""
        ),
        aic = round(!!rlang::sym("aic"), 2),
        bic = round(!!rlang::sym("bic"), 2),
        loglik = round(!!rlang::sym("loglik"), 2)
      ) %>%
      dplyr::mutate_if(is.numeric, as.character) %>%
      dplyr::select(
        "Growth function" = !!rlang::sym("model_function"),
        "Regression type" = !!rlang::sym("model_type"),
        "Number of observations" = !!rlang::sym("number_observations"),
        "Lower asymptote estimate [95% CI]" = !!rlang::sym("lower_asy_ci"),
        "Upper asymptote estimate [95% CI]" = !!rlang::sym("upper_asy_ci"),
        "Inflection point estimate [95% CI]" = !!rlang::sym("inflection_ci"),
        "Rate constant estimate [95% CI]" = !!rlang::sym("rate_ci"),
        !!paste("Doubling time estimate (",
                time_unit,
                ") [95% CI]",
                sep = "") := !!rlang::sym("double_time_ci"),
        "Akaike information criterion (AIC)" = !!rlang::sym("aic"),
        "Bayesian information criterion (BIC)" = !!rlang::sym("bic"),
        "Log likelihood" = !!rlang::sym("loglik")
      )
    # Transpose and convert to a dataframe
    model_summary_long <- as.data.frame(t(model_summary_long))

    # Convert rownames to a column and rename to Value
    model_summary_long <- model_summary_long %>%
      tibble::rownames_to_column(var = "Variable") %>%
      dplyr::rename("Value" = !!rlang::sym("V1"))
  }

  # Extract original data used to fit ls_model
  model_residual_data <- data_frame
  # Extract residuals and create empty variable columns
  model_residual_data <- model_residual_data %>%
    dplyr::mutate(
      pop_fit_value = predict(ls_model),
      pop_resid = stats::residuals(ls_model),
      pop_resid_quant =
        stats::qqnorm(!!rlang::sym("pop_resid"), plot.it = FALSE)$x,
      pop_stand_resid = stats::residuals(ls_model, type = "pearson"),
      pop_stand_resid_quant =
        stats::qqnorm(!!rlang::sym("pop_stand_resid"), plot.it = FALSE)$x
    )

  # Extract time points
  times <- data_frame %>%
    dplyr::select(!!rlang::sym("time")) %>%
    dplyr::distinct()

  # Calculate median and prediction intervals with investr
  invest_predict <- investr::predFit(
    ls_model,
    interval = "prediction",
    newdata = times)

  # Create d
  simulated_data <- times %>%
    dplyr::mutate(
    sim_pop_pred_value = invest_predict[,"fit"],
    sim_pop_pred_lb = invest_predict[,"lwr"],
    sim_pop_pred_ub = invest_predict[,"upr"]
  )

  # Create a list of wide and long ls_model summary datasets
  model_summary_list <- list(
    "model_summary_wide" = model_summary_wide,
    "model_summary_long" = model_summary_long,
    "model_residual_data" = model_residual_data,
    "model_sim_pred_data" = simulated_data
  )

  # Return the model_summary_list
  return(model_summary_list)
}
