#' Fit a growth function using mixed-effects regression modeling
#'
#'@description
#''growth_curve_model_fit()' fits a mixed-effects model to a data frame based
#'on a user-defined function to account for clustering.
#'
#'
#' @param data_frame A data frame object that at minimum contains three
#' variables:
#'\itemize{
#'  \item cluster - a character type variable used to specify how observations
#'  are nested or grouped by a particular cluster. Note if using a
#'  least-squares model, please fill in all values of cluster with a single
#'  dummy character string, do NOT leave blank.
#'  \item time - a numeric type variable used for measuring time such as
#'  minutes, hours, or days
#'  \item growth_metric - a numeric type variable used for measuring growth
#'  over time such as cell count or confluency
#'}
#' @param function_type A character string specifying the function for
#' modeling the shape of the growth. Options include "exponential", "linear",
#' "logistic", or "gompertz".
#' @param model_type A character string specifying the type of regression
#' model to be used. If "mixed" a mixed-effects regression model will be used
#' with fixed and random effects to account for clustering. Defaults to "mixed".
#' @param fixed_rate A logical value specifying whether the rate constant
#' of the function should be treated as a fixed effect (TRUE) or random
#' effect (FALSE). Defaults to TRUE
#' @param num_chains A numeric value specifying the number of chains to run
#' in parallel in the MCMC algorithm of saemix. Defaults to 1.
#'
#' @return Returns a model object of class 'saemix' when a mixed-effects
#' model is specified or a model object of class 'nls' if a least-squares
#' model is specified.
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange count filter
#' @export
#'
#' @examples
#' # Load example data (exponential data)
#' data(exp_mixed_data)
#' # Fit an mixed-effects growth model to the data
#' exp_mixed_model <- growth_curve_model_fit(data_frame = exp_mixed_data, function_type = "exponential")
#' # Summarize the data by creating a summary list object
#' exp_mixed_model_summary <- summarize_growth_model(data_frame = exp_mixed_data,
#' growth_model_object = exp_mixed_model,
#' model_type = "mixed",
#' function_type = "exponential")
#' # Create flextable object from the summary list object for documentation
#' exp_model_table <- growth_model_summary_table(growth_model_summary_list = exp_mixed_model_summary)
#' print(exp_model_table)
#' # Create growth vs time plot of data with fitted values (plot_type = 2), adjust aesthetics and parameters as desired
#' exp_growth_plot <- growth_vs_time_plot(growth_model_summary_list = exp_mixed_model_summary,
#' model_type = "mixed",
#' plot_type = 2)
#' print(exp_growth_plot)
#' # Check residuals and model assumptions
#' residual_diag_plot <- growth_model_residual_plots(growth_model_summary_list = exp_mixed_model_summary,
#' model_type = "mixed",
#' residual_type = "conditional")
#' print(residual_diag_plot)
#' # Create a plot of fixed-effects bootstrapped 95%CI with the doubling time estimates from the exp_mixed_model annotated on the plot
#' exp_ci_plot <- growth_model_boot_ci_curve(growth_model_object = exp_mixed_model,
#' growth_model_summary_list = exp_mixed_model_summary,
#' model_type = "mixed",
#' annotate_value = "double_time",
#' return = "plot")
#' print(exp_ci_plot)
growth_curve_model_fit <- function(data_frame,
                                   function_type = "exponential",
                                   model_type = "mixed",
                                   fixed_rate = TRUE,
                                   num_chains = 1) {
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
    num_chains >= 1
  )

  # Remove missing values from cluster, time, and growth_metric variables
  data_frame <- data_frame %>%
    dplyr::filter(
      !is.na(cluster),
      !is.na(time),
      !is.na(growth_metric)
    )

  # If data_frame has < 3 data points, stop function and print message_low_n
  if (nrow(data_frame) < 3) {
    message_low_n <- paste0(
      "After removing missing values, input data contains ", nrow(data_frame),
      " rows. \nPlease check initial data_frame input and ensure the variables cluster, time, and growth_metric are completed, cluster contains at least 1 unique character value for observations, and data_frame contains at least 3 observations."
    )
    stop(message_low_n)
  }

  # Check number of cluster
  cluster_num <- data_frame %>%
    dplyr::filter(!is.na(cluster)) %>%
    dplyr::count(cluster) %>%
    nrow() %>%
    as.numeric()

  # If number of clusters is <= 1 and model_type is specified as 'mixed', change model_type to 'least-squares'
  if (cluster_num <= 1 &
    model_type == "mixed") {
    warn_message <- paste(
      "Warning: number of clusters is", cluster_num,
      "and arguement 'model_type' is set to TRUE. Due to lack of multiple clusters, 'model_type' has been set to FALSE and a least squares model will be applied"
    )
    message(warn_message)
    model_type <- "least-squares"
  } else if (model_type == "mixed") {
    cat("Number of clusters:", cluster_num, "\n")
    cat("Number of unique time points:", length(unique(data_frame$time)), "\n")
    cat("Number of observations:", nrow(data_frame), "\n")
  }

  # Print data info model_type is "least-squares
  if (model_type == "least-squares") {
    cat("Number of unique time points:", length(unique(data_frame$time)), "\n")
    cat("Number of observations:", nrow(data_frame), "\n")
  }

  # Arrange data_frame by time and cluster
  data_frame <- data_frame %>%
    dplyr::arrange(time, cluster)

  # If exponential model is chosen
  if (function_type == "exponential") {
    model <- exponential_mixed_model(
      data_frame = data_frame,
      fixed_rate = fixed_rate,
      model_type = model_type,
      num_chains = num_chains
    )
  }
  # If linear model is chosen
  if (function_type == "linear") {
    model <- linear_mixed_model(
      data_frame = data_frame,
      fixed_rate = fixed_rate,
      model_type = model_type,
      num_chains = num_chains
    )
  }
  # If logistic model is selected
  if (function_type == "logistic") {
    model <- logistic_mixed_model(
      data_frame = data_frame,
      fixed_rate = fixed_rate,
      model_type = model_type,
      num_chains = num_chains
    )
  }
  # If gompertz model is selected
  if (function_type == "gompertz") {
    model <- gompertz_mixed_model(
      data_frame = data_frame,
      fixed_rate = fixed_rate,
      model_type = model_type,
      num_chains = num_chains
    )
  }

  # Return the model
  return(model)
}
