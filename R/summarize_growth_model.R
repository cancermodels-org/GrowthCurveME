#' Summarize growth model object and data
#'
#' @description
#' This function is used to create a list object of
#' data frames based on a user's input data and output
#' growth model object from \code{\link{growth_curve_model_fit}}.
#' The list object (referred to in this package as 'growth_model_summary_list')
#' can be used to extract model predicted values, residuals, and can be
#' inputted into supporting functions from GrowthCurveME to
#' generate plots and perform model diagnostics.
#'
#'
#' @inheritParams growth_curve_model_fit
#' @param model_type A character string specifying the model_type that was
#' fit using the \code{\link{growth_curve_model_fit}} function. Options
#' include either "mixed" or "least-squares". Defaults to "mixed".
#' @param growth_model_object The model object that is created using
#' the \code{\link{growth_curve_model_fit}}
#'
#' @return A list object with the following data frames within the list:
#'\itemize{
#'  \item model_summary_wide - a data frame with 1 row containing
#'  key model estimates, doubling-time, and model metrics depending
#'  on the model_type and function_type specified
#'  \item model_summary_long - a data frame that is a long dataset version of
#'  'model_summary_wide' that can be used to generate a table of the model
#'  results (see function \code{\link{growth_model_summary_table}})
#'  \item model_residual_data - a data frame containing the original data
#'  frame values as well as predicted values, residuals, and theoretical
#'  quantiles of the residuals depending on the model_type selected
#'  (see functions \code{\link{growth_model_residual_plots}} and
#'  \code{\link{growth_vs_time_plot}})
#'  \item model_sim_pred_data - a data frame with estimates and 95% prediction
#'  intervals (not to be confused with the 95% confidence intervals calculated
#'  from the model estimates), for mixed-effects models, values are calculated
#'  as the median estimate and the 2.5th and 97.5th percentiles of the
#'  simulated data from the saemix model at each time point (see
#'  \code{\link[saemix]{compute.sres}} and \code{\link[saemix]{plot}} with
#'  plot.type = "vpc"). For least-squares models, prediction intervals are
#'  calculated through Taylor-series approximations using the
#'  \code{\link[investr]{predFit}} function.
#'}
#'
#' @seealso
#' \code{\link{growth_curve_model_fit}}
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange filter
#' @importFrom rlang sym
#' @export
#'
#' @examples
#' # Load example data (exponential data)
#' data(exp_mixed_data)
#' # Fit an mixed-effects growth model to the data
#' exp_mixed_model <- growth_curve_model_fit(
#' data_frame = exp_mixed_data,
#' function_type = "exponential",
#' return_summary = FALSE)
#' # Summarize the data by creating a summary list object
#' exp_mixed_model_summary <- summarize_growth_model(
#' data_frame = exp_mixed_data,
#' growth_model_object = exp_mixed_model,
#' model_type = "mixed",
#' function_type = "exponential",
#' time_unit = "hours")
#' # Extracting a data frame from the list object
#' model_summary_wide <- exp_mixed_model_summary[["model_summary_wide"]]
summarize_growth_model <- function(data_frame,
                                   growth_model_object,
                                   model_type = "mixed",
                                   function_type = "exponential",
                                   fixed_rate = TRUE,
                                   time_unit = "hours") {
  # Check initial inputs
  stopifnot(
    "cluster" %in% colnames(data_frame),
    "time" %in% colnames(data_frame),
    "growth_metric" %in% colnames(data_frame),
    is.numeric(data_frame$growth_metric),
    is.numeric(data_frame$time),
    model_type %in% c("mixed", "least-squares"),
    function_type %in% c(
      "exponential", "linear", "logistic",
      "gompertz"
    ),
    is.logical(fixed_rate),
    inherits(growth_model_object, c("SaemixObject", "saemix", "nls"))
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

  # Summarize data based on model_type
  if (model_type == "mixed") {
    summary_object <- summarize_growth_model_mixed(
      data_frame = data_frame,
      mixed_growth_model = growth_model_object,
      function_type = function_type,
      fixed_rate = fixed_rate,
      time_unit = time_unit
    )
  } else {
    summary_object <- summarize_growth_model_ls(
      data_frame = data_frame,
      ls_model = growth_model_object,
      function_type = function_type,
      time_unit = time_unit
    )
  }

  return(summary_object)
}
