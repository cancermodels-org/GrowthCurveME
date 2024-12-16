#' Fit a growth function using mixed-effects regression modeling
#'
#'@description
#'This function fits a mixed-effects model to a data frame based
#'on a user-defined function to account for clustering.
#'
#'
#' @param data_frame A data frame object that at minimum contains three
#' variables:
#'\itemize{
#'  \item cluster - a character type variable used to specify how observations
#'  are nested or grouped by a particular cluster. Note if using a
#'  least-squares model, please fill in cluster values with a single repetitive
#'  dummy variable (e.g., '1'), do not leave blank.
#'  \item time - a numeric type variable used for measuring time such as
#'  minutes, hours, or days
#'  \item growth_metric - a numeric type variable used for measuring growth
#'  over time such as cell count or confluency
#'}
#' @param function_type A character string specifying the function for
#' modeling the shape of the growth. Options include "exponential", "linear",
#' "logistic", or "gompertz".
#' @param model_type A character string specifying the type of regression
#' model to be used. If 'mixed', a mixed-effects regression model will be used
#' with fixed and random-effects to account for clustering. For
#' 'least-squares', a least-squares regression model with only fixed-effects
#' is applied. Defaults to "mixed".
#' @param fixed_rate A logical value specifying whether the rate constant
#' of the function should be treated as a fixed effect (TRUE) or random
#' effect (FALSE). Defaults to TRUE
#' @param num_chains A numeric value specifying the number of chains to run
#' in parallel in the MCMC algorithm of saemix. Increasing the number of chains
#' may improve convergence but may also increase the computational time.
#' Defaults to 1.
#' @param time_unit A character string specifying the units in which time is
#' measured in. Defaults to "hours"
#' @param return_summary A logical value specifying whether to return the
#' growth_model_summary_list when TRUE (list object containing summarized data)
#' or the object model object when FALSE. Defaults to TRUE.
#' @param seed A numeric value specifying a seed number to reproduce the
#' random starting values sampled within the function. Defaults to NULL.
#' @param verbose A logical value specifying whether print statements will
#' print in the console. Defaults to TRUE.
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
#'  \item simulated_data - a data frame with the 95% prediction intervals
#'  calculated using the median for the estimate and the 2.5th and 97.5th
#'  percentiles of the simulated data at each time point (not to be
#'  confused with the 95% confidence intervals calculated from the model
#'  estimates). See \code{\link{summarize_growth_model}}.
#'}
#' Note when return_summary is FALSE, will return a model object of class
#' 'SaemixObject' when a mixed-effects model is specified or a model object of
#' class 'nls' if a least-squares model is specified.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange count filter
#' @importFrom rlang sym
#' @export
#'
#' @examples
#' # Load example data (exponential data)
#' data(exp_mixed_data)
#' # Fit an mixed-effects growth model to the data and return summary
#' exp_mixed_model_summary <- growth_curve_model_fit(
#' data_frame = exp_mixed_data,
#' function_type = "exponential",
#' verbose = FALSE)
#' # Create flextable object from the summary list object for documentation
#' exp_model_table <- growth_model_summary_table(
#' growth_model_summary_list = exp_mixed_model_summary)
#' exp_model_table
#' # Create growth vs time plot of data with fitted values (plot_type = 2),
#' # adjust aesthetics and parameters as desired
#' exp_growth_plot <- growth_vs_time_plot(
#' growth_model_summary_list = exp_mixed_model_summary,
#' plot_type = 2)
#' print(exp_growth_plot)
#' # Check residuals and model assumptions
#' residual_diag_plot <- growth_model_residual_plots(
#' growth_model_summary_list = exp_mixed_model_summary)
#' print(residual_diag_plot)
growth_curve_model_fit <- function(data_frame,
                                   function_type = "exponential",
                                   model_type = "mixed",
                                   fixed_rate = TRUE,
                                   num_chains = 1,
                                   time_unit = "hours",
                                   return_summary = TRUE,
                                   seed = NULL,
                                   verbose = TRUE) {
  # Check initial data frame inputs
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
    model_type %in% c("mixed", "least-squares"),
    is.logical(fixed_rate),
    is.numeric(num_chains),
    num_chains >= 1,
    is.character(time_unit),
    is.logical(return_summary),
    (is.null(seed) | is.numeric(seed))
  )

  # Remove missing values from cluster, time, and growth_metric variables
  data_frame <- data_frame %>%
    dplyr::filter(
      !is.na(!!rlang::sym("cluster")),
      !is.na(!!rlang::sym("time")),
      !is.na(!!rlang::sym("growth_metric"))
    )

  # If data_frame has < 3 data points, stop function and print message_low_n
  if (nrow(data_frame) < 3) {
    message_low_n <- paste0(
      "After removing missing values, input data contains ", nrow(data_frame),
      " rows. \nPlease check initial data_frame input and ensure the",
      " variables cluster, time, and growth_metric are completed, cluster",
      " contains at least 1 unique character value for observations, and",
      " data_frame contains at least 3 observations."
    )
    stop(message_low_n)
  }

  # Check number of cluster
  cluster_num <- data_frame %>%
    dplyr::filter(!is.na(!!rlang::sym("cluster"))) %>%
    dplyr::count(!!rlang::sym("cluster")) %>%
    nrow() %>%
    as.numeric()

  # If number of clusters is <= 1 and model_type is specified as 'mixed',
  # change model_type to 'least-squares'
  if (cluster_num <= 1 &
    model_type == "mixed") {
    warn_message <- paste0(
      "Warning: number of clusters is ", cluster_num,
      "and argument 'model_type' is set to 'mixed'. Due to lack of multiple",
      " clusters, 'model_type' has been set to 'least-squares'")
    message(warn_message)
    model_type <- "least-squares"
  } else if (model_type == "mixed" &
             verbose == TRUE) {
    cat("Number of clusters:", cluster_num, "\n")
    cat("Number of unique time points:", length(unique(data_frame$time)), "\n")
    cat("Number of observations:", nrow(data_frame), "\n")
  }

  # Print data info model_type is "least-squares
  if (model_type == "least-squares" &
      verbose == TRUE) {
    cat("Number of unique time points:", length(unique(data_frame$time)), "\n")
    cat("Number of observations:", nrow(data_frame), "\n")
  }

  # Arrange data_frame by cluster and time
  data_frame <- data_frame %>%
    dplyr::arrange(!!rlang::sym("cluster"), !!rlang::sym("time"))

  # If exponential model is chosen
  if (function_type == "exponential") {
    model <- exponential_mixed_model(
      data_frame = data_frame,
      fixed_rate = fixed_rate,
      model_type = model_type,
      num_chains = num_chains,
      seed = seed
    )
  }
  # If linear model is chosen
  if (function_type == "linear") {
    model <- linear_mixed_model(
      data_frame = data_frame,
      fixed_rate = fixed_rate,
      model_type = model_type,
      num_chains = num_chains,
      seed = seed
    )
  }
  # If logistic model is selected
  if (function_type == "logistic") {
    model <- logistic_mixed_model(
      data_frame = data_frame,
      fixed_rate = fixed_rate,
      model_type = model_type,
      num_chains = num_chains,
      seed = seed
    )
  }
  # If gompertz model is selected
  if (function_type == "gompertz") {
    model <- gompertz_mixed_model(
      data_frame = data_frame,
      fixed_rate = fixed_rate,
      model_type = model_type,
      num_chains = num_chains,
      seed = seed
    )
  }

  # Return either the growth_model_summary_list object or the model object
  if(return_summary == TRUE){
    growth_model_summary_list <- summarize_growth_model(
      data_frame = data_frame,
      growth_model_object = model,
      model_type = model_type,
      function_type = function_type,
      fixed_rate = fixed_rate,
      time_unit = time_unit
    )
    return(growth_model_summary_list)
  }else{
    return(model)
  }
}
