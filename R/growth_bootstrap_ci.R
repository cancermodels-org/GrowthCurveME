#' Create bootstrap estimates and 95% confidence intervals for mixed-effects
#' and/or least-squares models at each time-point
#'
#' @description
#' This function leverages the \code{\link[saemix]{saemix.bootstrap}} function
#' for mixed-effects models and the \code{\link[nlraa]{predict_nls}} function
#' for least-squares models to compute bootstrapped 95% confidence intervals for
#' each time-point for graphical purposes. Estimates of the fixed-effect
#' values are calculated based on the median (50th percentile) of the
#' simulated bootstrap data, with the 95% confidence interval constructed
#' from the 2.5th and 97.5th percentiles.
#'
#'
#' @inheritParams growth_curve_model_fit
#' @param growth_model_object A saemix or nls type model object that is
#' created using \code{\link{growth_curve_model_fit}} when
#' return_summary = FALSE
#' @param growth_model_summary_list A list object created by the
#' \code{\link{growth_curve_model_fit}} function when return_summary = TRUE.
#' @param boot_n_sim A numeric value specifying the number of bootstrap
#' simulations to be performed. See \code{\link[saemix]{saemix.bootstrap}}
#' for mixed-effects models and \code{\link[nlraa]{predict_nls}} for
#' least-squares models. Defaults to 200.
#' @param mix_boot_method For mixed-effects models, a character string
#' specifying the bootstrap algorithm to use. Options include "case",
#' "residual", "parametric" or "conditional". Defaults to "case". See
#' \code{\link[saemix]{saemix.bootstrap}} for more details.
#'
#' @return An appended version of growth_model_summary_list with a fourth
#' data frame titled "boot_sim"
#' @seealso
#' \code{\link{growth_curve_model_fit}}
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange bind_rows filter group_by mutate pull select
#' summarize
#' @importFrom saemix saemix.bootstrap
#' @importFrom nlraa predict_nls
#' @importFrom stats quantile
#' @export
#'
#' @examples
#' # Fit an mixed-effects growth model to the data, return summary
#' # and bootstrap estimates (boot_n_sim set to 5 for speed, please set to 200)
#' exp_mixed_model_summary <- growth_curve_model_fit(
#'   data_frame = exp_mixed_data,
#'   function_type = "exponential",
#'   bootstrap_time = TRUE,
#'   boot_n_sim = 5
#' )
growth_bootstrap_ci <- function(data_frame,
                               growth_model_object,
                               growth_model_summary_list,
                               boot_n_sim = 200,
                               mix_boot_method = "case") {
  # Check inputs
  stopifnot(
    inherits(growth_model_object, c("SaemixObject", "saemix", "nls")),
    is.list(growth_model_summary_list),
    exists("model_summary_wide", growth_model_summary_list),
    exists("model_residual_data", growth_model_summary_list),
    is.numeric(boot_n_sim),
    boot_n_sim >= 1,
    is.character(mix_boot_method),
    mix_boot_method %in% c(
      "case", "residual",
      "parametric", "conditional"
    )
  )

  # Prepare data frame
  data_frame <- data_frame %>%
    dplyr::filter(
      !is.na(!!rlang::sym("cluster")),
      !is.na(!!rlang::sym("time")),
      !is.na(!!rlang::sym("growth_metric"))
    ) %>%
    dplyr::arrange(!!rlang::sym("time"))

  # Extract the model type
  model_type <- growth_model_summary_list[["model_summary_wide"]] %>%
    dplyr::pull(!!rlang::sym("model_type")) %>%
    as.character()

  # Extract function type
  model_function <- growth_model_summary_list[["model_summary_wide"]] %>%
    dplyr::pull(!!rlang::sym("model_function")) %>%
    as.character()

  # Extract the model residual data from growth_model_summary_list
  data_frame <- growth_model_summary_list[["model_residual_data"]]

  # If model_type == "mixed-effects" use saemix.bootstrap
  if (model_type == "mixed-effects") {
    # Print message
    cat(paste0(
      "Performing bootstrap calculations on mixed-effects model.",
      " Calculations may take several minutes depending on number of",
      " bootstrap simulations specified."
    ))
    # Perform bootstrap calculations
    boot_parameters <- saemix::saemix.bootstrap(
      growth_model_object,
      method = mix_boot_method,
      nboot = boot_n_sim
    )

    # Extract time points
    times <- data_frame %>%
      dplyr::pull(!!rlang::sym("time")) %>%
      unique()

    # Prepare an empty data frame
    simulated_data <- data.frame(
      replicate = integer(),
      time = numeric(),
      growth_metric = numeric()
    )

    # Exponential function
    if (model_function == "exponential") {
      for (a in 1:nrow(boot_parameters)) {
        # Extract model parameters
        intercept <- boot_parameters[a, "intercept"]
        rate <- boot_parameters[a, "rate"]

        # Calculate predicted growth metrics
        growth_metric <- intercept * exp(rate * times)

        # Create temporary dataset
        data_temp <- data.frame(
          replicate = boot_parameters[a, "Replicate"],
          time = times,
          growth_metric = growth_metric
        )

        # Bind data to simulated_data frame
        simulated_data <- simulated_data %>%
          dplyr::bind_rows(data_temp)

        # Remove temporary object
        rm(
          intercept, rate, growth_metric,
          data_temp
        )
      }
    }

    # Linear function
    if (model_function == "linear") {
      for (a in 1:nrow(boot_parameters)) {
        # Extract model parameters
        intercept <- boot_parameters[a, "intercept"]
        rate <- boot_parameters[a, "rate"]

        # Calculate predicted growth metrics
        growth_metric <- intercept + (rate * times)

        # Create temporary dataset
        data_temp <- data.frame(
          replicate = boot_parameters[a, "Replicate"],
          time = times,
          growth_metric = growth_metric
        )

        # Bind data to simulated_data frame
        simulated_data <- simulated_data %>%
          dplyr::bind_rows(data_temp)

        # Remove temporary object
        rm(
          intercept, rate, growth_metric,
          data_temp
        )
      }
    }

    # Logistic function
    if (model_function == "logistic") {
      for (a in 1:nrow(boot_parameters)) {
        # Extract model parameters
        lower_asy <- boot_parameters[a, "lower_asy"]
        upper_asy <- boot_parameters[a, "upper_asy"]
        rate <- boot_parameters[a, "rate"]
        inflection <- boot_parameters[a, "inflection"]

        # Calculate predicted growth metrics
        growth_metric <- lower_asy +
          (upper_asy - lower_asy) / (1 + exp(-rate * (times - inflection)))

        # Create temporary dataset
        data_temp <- data.frame(
          replicate = boot_parameters[a, "Replicate"],
          time = times,
          growth_metric = growth_metric
        )

        # Bind data to simulated_data frame
        simulated_data <- simulated_data %>%
          dplyr::bind_rows(data_temp)

        # Remove temporary object
        rm(
          lower_asy, upper_asy, rate, inflection,
          growth_metric, data_temp
        )
      }
    }

    # Gompertz function
    if (model_function == "gompertz") {
      for (a in 1:nrow(boot_parameters)) {
        # Extract model parameters
        lower_asy <- boot_parameters[a, "lower_asy"]
        upper_asy <- boot_parameters[a, "upper_asy"]
        rate <- boot_parameters[a, "rate"]
        inflection <- boot_parameters[a, "inflection"]

        # Calculate predicted growth metrics
        growth_metric <- lower_asy +
          (upper_asy - lower_asy) * exp(-exp(-rate * (times - inflection)))

        # Create temporary dataset
        data_temp <- data.frame(
          replicate = boot_parameters[a, "Replicate"],
          time = times,
          growth_metric = growth_metric
        )

        # Bind data to simulated_data frame
        simulated_data <- simulated_data %>%
          dplyr::bind_rows(data_temp)

        # Remove temporary object
        rm(
          lower_asy, upper_asy, rate, inflection,
          growth_metric, data_temp
        )
      }
    }

    # Calculate lower (2.5 percentile) and upper (97.5 percentile)
    simulated_data <- simulated_data %>%
      dplyr::group_by(!!rlang::sym("time")) %>%
      dplyr::summarize(
        sim_pop_pred_value = stats::quantile(growth_metric, 0.50),
        sim_pop_pred_lb = stats::quantile(growth_metric, 0.025),
        sim_pop_pred_ub = stats::quantile(growth_metric, 0.975)
      )

    # If model_type is "least-squares"
  } else {
    cat(
      paste0(
        "Performing bootstrap calculations on least-squares model",
        ", calculations may take several minutes depending on number of",
        " bootstrap simulations specified."
      )
    )

    # Extract time points
    times <- data_frame %>%
      dplyr::pull(!!rlang::sym("time")) %>%
      unique()

    # Utilize nlraa package predict_nls function to generate bootstraps
    simulated_data <- nlraa::predict_nls(
      growth_model_object,
      interval = "confidence",
      nsim = boot_n_sim,
      newdata = data_frame
    )

    # Select and rename variable from nlraa predict_nls
    simulated_data <- data.frame(simulated_data) %>%
      dplyr::select(
        sim_pop_pred_value = !!rlang::sym("Estimate"),
        sim_pop_pred_lb = !!rlang::sym("Q2.5"),
        sim_pop_pred_ub = !!rlang::sym("Q97.5")
      ) %>%
      dplyr::distinct()

    simulated_data <- simulated_data %>%
      dplyr::mutate(time = times) %>%
      dplyr::select(dplyr::all_of(
        c(
          "time", "sim_pop_pred_value",
          "sim_pop_pred_lb", "sim_pop_pred_ub"
        )
      ))
  }

  # Add simulated data to growth_model_summary_list
  growth_model_summary_list[["boot_sim"]] <- simulated_data

  # Return summary list
  return(growth_model_summary_list)
}
