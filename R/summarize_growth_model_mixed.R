#' Summarize mixed-effects growth model object and data
#'
#' @description
#' 'summarize_growth_model_mixed()' is a function used within the \code{\link{summarize_growth_model}} function to create a list object of data frames based on a user's input data frame and outputed mixed-effects growth model object from \code{\link{growth_curve_model_fit}}.
#' The list object (referred to in this package as 'growth_model_summary_list') can be used to extract model predicted values, residuals, and can be in-putted into supporting functions from GrowthCurveME to generate plots and perform model diagnostics.
#'
#'
#' @inheritParams growth_curve_model_fit
#' @param mixed_growth_model The mixed-effects model object that is created using the 'growth_curve_model_fit()'
#'
#' @inherit summarize_growth_model return
#' @seealso \code{\link{growth_curve_model_fit}} \code{\link{summarize_growth_model}}
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange bind_rows bind_cols case_when filter mutate
#' mutate_if rename select summarize ungroup
#' @importFrom tibble rownames_to_column tibble
#' @importFrom nlme intervals VarCorr
#' @importFrom stats qqnorm residuals
#' @importFrom rlang sym
#' @export
#'
#' @examples
#' # Load example data (exponential data)
#' data(exp_mixed_data)
#' # Fit an mixed-effects growth model to the data
#' exp_mixed_model <- growth_curve_model_fit(data_frame = exp_mixed_data, function_type = "exponential")
#' # Summarize the data by creating a summary list object with main 'summarize_growth_model()' function
#' exp_mixed_model_summary <- summarize_growth_model(data_frame = exp_mixed_data, growth_model_object = exp_mixed_model, model_type = "mixed", function_type = "exponential")
#' # Summarize the data by creating a summary list object with main 'summarize_growth_model()' function
#' exp_mixed_model_summary <- summarize_growth_model_mixed(data_frame = exp_mixed_data, mixed_growth_model = exp_mixed_model, function_type = "exponential")
#' # Extracting a data frame from the list object
#' model_summary_wide <- exp_mixed_model_summary[["model_summary_wide"]]
summarize_growth_model_mixed <- function(data_frame,
                                         mixed_growth_model,
                                         function_type = "exponential",
                                         fixed_rate = TRUE) {
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
    is.logical(fixed_rate),
    inherits(mixed_growth_model, c("SaemixObject", "saemix"))
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

  # If regression mixed_growth_model function_type is exponential or linear
  if (function_type %in% c("exponential", "linear")) {
    # Create wide data set to store variables and components within it
    model_summary_wide <- tibble::tibble(
      model_function = function_type,
      model_type = "mixed-effects",
      number_observations = data_frame %>%
        nrow() %>% as.numeric(),
      number_clusters = data_frame %>%
        dplyr::count(!!rlang::sym("cluster"))
      %>% nrow() %>% as.numeric(),
      fixed_rate = as.character(fixed_rate),
      intercept_est = mixed_growth_model@results@conf.int[1, 2],
      intercept_se = mixed_growth_model@results@conf.int[1, 3],
      intercept_lb = mixed_growth_model@results@conf.int[1, 5],
      intercept_ub = mixed_growth_model@results@conf.int[1, 6],
      rate_est = mixed_growth_model@results@conf.int[2, 2],
      rate_se = mixed_growth_model@results@conf.int[2, 3],
      rate_lb = mixed_growth_model@results@conf.int[2, 5],
      rate_ub = mixed_growth_model@results@conf.int[2, 6],
      double_time_hours_est = log(2) / rate_est,
      double_time_hours_lb = log(2) / rate_ub,
      double_time_hours_ub = log(2) / rate_lb,
      AIC = mixed_growth_model@results@aic.lin,
      BIC = mixed_growth_model@results@bic.lin,
      loglik = mixed_growth_model@results@ll.lin,
    )

    # Round and create tidy variables for convert to long dataset
    # for table figure
    model_summary_long <- model_summary_wide %>%
      dplyr::mutate(
        intercept_est = round(!!rlang::sym("intercept_est"), 2),
        intercept_lb = round(!!rlang::sym("intercept_lb"), 2),
        intercept_ub = round(!!rlang::sym("intercept_ub"), 2),
        intercept_ci = paste(!!rlang::sym("intercept_est"),
                             " [", !!rlang::sym("intercept_lb"),
                             ",",
                             !!rlang::sym("intercept_ub"),
                             "]",
                             sep = ""
        ),
        rate_est = round(!!rlang::sym("rate_est"), 6),
        rate_lb = round(!!rlang::sym("rate_lb"), 6),
        rate_ub = round(!!rlang::sym("rate_ub"), 6),
        rate_ci = paste(!!rlang::sym("rate_est"),
                        " [", !!rlang::sym("rate_lb"),
                        ",", !!rlang::sym("rate_ub"),
                        "]",
                        sep = ""),
        double_time_hours_est = round(!!rlang::sym("double_time_hours_est"), 2),
        double_time_hours_lb = round(!!rlang::sym("double_time_hours_lb"), 2),
        double_time_hours_ub = round(!!rlang::sym("double_time_hours_ub"), 2),
        double_time_ci = paste(!!rlang::sym("double_time_hours_est"),
                               " [", !!rlang::sym("double_time_hours_lb"),
                               ",",
          !!rlang::sym("double_time_hours_ub"),
          "]",
          sep = ""
        ),
        aic = round(!!rlang::sym("AIC"), 2),
        bic = round(!!rlang::sym("BIC"), 2),
        loglik = round(!!rlang::sym("loglik"), 2)
      ) %>%
      dplyr::mutate_if(is.numeric, as.character) %>%
      dplyr::select(
        "Model function" = !!rlang::sym("model_function"),
        "Model type" = !!rlang::sym("model_type"),
        "Number of observations" = !!rlang::sym("number_observations"),
        "Number of clusters" = !!rlang::sym("number_clusters"),
        "Intercept [95% CI]" = !!rlang::sym("intercept_ci"),
        "Rate constant [95% CI]" = !!rlang::sym("rate_ci"),
        "Doubling time estimate [95% CI]" = !!rlang::sym("double_time_ci"),
        "Akaike information criterion (AIC)" = !!rlang::sym("aic"),
        "Bayesian information criterion (BIC)" = !!rlang::sym("bic"),
        "Log likelihood" = !!rlang::sym("loglik"),
      )

    # Transpose and convert to a dataframe
    model_summary_long <- as.data.frame(t(model_summary_long))

    # Convert rownames to a column and rename to Value
    model_summary_long <- model_summary_long %>%
      tibble::rownames_to_column(var = "Variable") %>%
      dplyr::rename("Value" = !!rlang::sym("V1"))

    # If fixed_rate is FALSE, add the random effect correlations
    if (fixed_rate == FALSE) {
      # Prepare random effects correlations
      model_random_corr <-
        as.data.frame(mixed_growth_model@results@conf.int) %>%
        dplyr::select(dplyr::all_of(c("name", "estimate"))) %>%
        dplyr::filter(stringr::str_detect(!!rlang::sym("name"), "Corr")) %>%
        dplyr::mutate(
          name = stringr::str_replace(!!rlang::sym("name"), "Corr.", "corr_"),
          name = stringr::str_replace(!!rlang::sym("name"), "\\.", "_")
        )

      # Create a wide dataset
      model_random_corr_wide <- model_random_corr %>%
        tidyr::pivot_wider(
          names_from = !!rlang::sym("name"),
          values_from = !!rlang::sym("estimate")
        )

      # Append model_summary_wide with model_random_corr_wide
      model_summary_wide <- model_summary_wide %>%
        dplyr::bind_cols(model_random_corr_wide)

      # Create random correlation long dataset
      model_random_corr_long <-
        as.data.frame(mixed_growth_model@results@conf.int) %>%
        dplyr::select(dplyr::all_of(c("name", "estimate"))) %>%
        dplyr::filter(stringr::str_detect(!!rlang::sym("name"), "Corr")) %>%
        dplyr::mutate(
          name = stringr::str_replace(!!rlang::sym("name"), "Corr.", "Corr: "),
          name = stringr::str_replace(!!rlang::sym("name"), "\\.", "-"),
          estimate = round(!!rlang::sym("estimate"), 4),
          estimate = as.character(!!rlang::sym("estimate"))
        ) %>%
        dplyr::rename(
          "Variable" = !!rlang::sym("name"),
          "Value" = !!rlang::sym("estimate")
        )

      # Append model_summary_long with model_random_corr_long
      model_summary_long <- model_summary_long %>%
        dplyr::bind_rows(model_random_corr_long)
    }
  }

  # If mixed_growth_model function_type is logistic or Gompertz
  if (function_type == "logistic" | function_type == "gompertz") {

    # Create wide data set to store variables and components within it
      model_summary_wide <- tibble::tibble(
        model_function = function_type,
        model_type = "mixed",
        number_observations = data_frame %>%
          nrow() %>% as.numeric(),
        number_clusters = data_frame %>%
          dplyr::count(!!rlang::sym("cluster"))
        %>% nrow() %>% as.numeric(),
        fixed_rate = as.character(fixed_rate),
        lower_asy_est = mixed_growth_model@results@conf.int[1, 2],
        lower_asy_se = mixed_growth_model@results@conf.int[1, 3],
        lower_asy_lb = mixed_growth_model@results@conf.int[1, 5],
        lower_asy_ub = mixed_growth_model@results@conf.int[1, 6],
        upper_asy_est = mixed_growth_model@results@conf.int[2, 2],
        upper_asy_se = mixed_growth_model@results@conf.int[2, 3],
        upper_asy_lb = mixed_growth_model@results@conf.int[2, 5],
        upper_asy_ub = mixed_growth_model@results@conf.int[2, 6],
        rate_est = mixed_growth_model@results@conf.int[3, 2],
        rate_se = mixed_growth_model@results@conf.int[3, 3],
        rate_lb = mixed_growth_model@results@conf.int[3, 5],
        rate_ub = mixed_growth_model@results@conf.int[3, 6],
        inflection_est = mixed_growth_model@results@conf.int[4, 2],
        inflection_se = mixed_growth_model@results@conf.int[4, 3],
        inflection_lb = mixed_growth_model@results@conf.int[4, 5],
        inflection_ub = mixed_growth_model@results@conf.int[4, 6],
        aic = mixed_growth_model@results@aic.lin,
        bic = mixed_growth_model@results@bic.lin,
        loglik = mixed_growth_model@results@ll.lin,
        double_time_hours = log(2) / rate_est,
        double_time_hours_lb = log(2) / rate_ub,
        double_time_hours_ub = log(2) / rate_lb,
      )

      # Round and create tidy variables for convert to long dataset for table figure
      model_summary_long <- model_summary_wide %>%
        dplyr::mutate(
          lower_asy_est = round(!!rlang::sym("lower_asy_est"), 2),
          lower_asy_lb = round(!!rlang::sym("lower_asy_lb"), 2),
          lower_asy_ub = round(!!rlang::sym("lower_asy_ub"), 2),
          lower_asy_ci = paste(!!rlang::sym("lower_asy_est"),
                               " [", !!rlang::sym("lower_asy_lb"),
                               ",", !!rlang::sym("lower_asy_ub"), "]",
                               sep = ""
          ),
          upper_asy_est = round(!!rlang::sym("upper_asy_est"), 2),
          upper_asy_lb = round(!!rlang::sym("upper_asy_lb"), 2),
          upper_asy_ub = round(!!rlang::sym("upper_asy_ub"), 2),
          upper_asy_ci = paste(!!rlang::sym("upper_asy_est"),
                               " [", !!rlang::sym("upper_asy_lb"),
                               ",", !!rlang::sym("upper_asy_ub"), "]",
                               sep = ""
          ),
          inflection_est = round(!!rlang::sym("inflection_est"), 2),
          inflection_lb = round(!!rlang::sym("inflection_lb"), 2),
          inflection_ub = round(!!rlang::sym("inflection_ub"), 2),
          inflection_ci = paste(!!rlang::sym("inflection_est"),
                                " [", !!rlang::sym("inflection_lb"),
                                ",", !!rlang::sym("inflection_ub"), "]",
                                sep = ""
          ),
          rate_est = round(!!rlang::sym("rate_est"), 6),
          rate_lb = round(!!rlang::sym("rate_lb"), 6),
          rate_ub = round(!!rlang::sym("rate_ub"), 6),
          rate_ci = paste(!!rlang::sym("rate_est"),
                          " [", !!rlang::sym("rate_lb"),
                          ",", !!rlang::sym("rate_ub"),
                          "]",
                          sep = ""),
          double_time_hours = round(!!rlang::sym("double_time_hours"), 2),
          double_time_hours_lb = round(!!rlang::sym("double_time_hours_lb"), 2),
          double_time_hours_ub = round(!!rlang::sym("double_time_hours_ub"), 2),
          double_time_ci = paste(!!rlang::sym("double_time_hours"),
                                 " [", !!rlang::sym("double_time_hours_lb"),
                                 ",",
                                 !!rlang::sym("double_time_hours_ub"),
                                 "]",
                                 sep = ""
          ),
          aic = round(!!rlang::sym("aic"), 2),
          bic = round(!!rlang::sym("bic"), 2),
          loglik = round(!!rlang::sym("loglik"), 2),
        ) %>%
        dplyr::mutate_if(is.numeric, as.character) %>%
        dplyr::select(
          "Model function" = !!rlang::sym("model_function"),
          "Model type" = !!rlang::sym("model_type"),
          "Number of observations" = !!rlang::sym("number_observations"),
          "Number of clusters" = !!rlang::sym("number_clusters"),
          "Lower asymptote estimate [95% CI]" = !!rlang::sym("lower_asy_ci"),
          "Upper asymptote estimate [95% CI]" = !!rlang::sym("upper_asy_ci"),
          "Inflection point estimate [95% CI]" = !!rlang::sym("inflection_ci"),
          "Rate constant estimate [95% CI]" = !!rlang::sym("rate_ci"),
          "Doubling time estimate [95% CI]" = !!rlang::sym("double_time_ci"),
          "Akaike information criterion (AIC)" = !!rlang::sym("aic"),
          "Bayesian information criterion (BIC)" = !!rlang::sym("bic"),
          "Log likelihood" = !!rlang::sym("loglik"),
        )

      # Transpose data and convert to dataframe
      model_summary_long <- as.data.frame(t(model_summary_long))
      # Move rownames to a column and rename to Value
      model_summary_long <- model_summary_long %>%
        tibble::rownames_to_column(var = "Variable") %>%
        dplyr::rename("Value" = !!rlang::sym("V1"))

      # Prepare random effects correlations
      model_random_corr <-
        as.data.frame(mixed_growth_model@results@conf.int) %>%
        dplyr::select(dplyr::all_of(c("name", "estimate"))) %>%
        dplyr::filter(stringr::str_detect(!!rlang::sym("name"), "Corr")) %>%
        dplyr::mutate(
          name = stringr::str_replace(!!rlang::sym("name"), "Corr.", "corr_"),
          name = stringr::str_replace(!!rlang::sym("name"), "\\.", "_")
        )

      # Create a wide dataset
      model_random_corr_wide <- model_random_corr %>%
        tidyr::pivot_wider(
          names_from = !!rlang::sym("name"),
          values_from = !!rlang::sym("estimate")
        )

      # Append model_summary_wide with model_random_corr_wide
      model_summary_wide <- model_summary_wide %>%
        dplyr::bind_cols(model_random_corr_wide)

      # Create random correlation long dataset
      model_random_corr_long <-
        as.data.frame(mixed_growth_model@results@conf.int) %>%
        dplyr::select(dplyr::all_of(c("name", "estimate"))) %>%
        dplyr::filter(stringr::str_detect(!!rlang::sym("name"), "Corr")) %>%
        dplyr::mutate(
          name = stringr::str_replace(!!rlang::sym("name"), "Corr.", "Corr: "),
          name = stringr::str_replace(!!rlang::sym("name"), "\\.", "-"),
          estimate = round(!!rlang::sym("estimate"), 4),
          estimate = as.character(!!rlang::sym("estimate"))
        ) %>%
        dplyr::rename(
          "Variable" = !!rlang::sym("name"),
          "Value" = !!rlang::sym("estimate")
        )

      # Append model_summary_long with model_random_corr_long
      model_summary_long <- model_summary_long %>%
        dplyr::bind_rows(model_random_corr_long)
  }

  # Join predictions and residuals with data
  model_residual_data <- data_frame %>%
    dplyr::bind_cols(mixed_growth_model@results@predictions)

  # Compute wres (weighted populatation residuals)
  model_update <- suppressMessages(
    suppressWarnings(
      saemix::compute.sres(mixed_growth_model)
    ))

  # Prepare data
  wres_model_data <- data_frame %>%
    dplyr::select(!!rlang::sym("cluster"),
                  !!rlang::sym("time"),
                  !!rlang::sym("growth_metric")) %>%
    dplyr::arrange(cluster, time) %>%
    dplyr::bind_cols(data.frame(wres = model_update@results@wres))

  # Join wres with model_residual_data and calculate theoretical quantiles
  # Used for residual diagnostics
  model_residual_data <- model_residual_data %>%
    dplyr::left_join(wres_model_data) %>%
    dplyr::mutate(
      pres = !!rlang::sym("growth_metric") - !!rlang::sym("ppred"),
      pres_theoretical_quantiles = stats::qqnorm(!!rlang::sym("pres"),
                                                 plot.it = FALSE)$x,
      ires_theoretical_quantiles = stats::qqnorm(!!rlang::sym("ires"),
                                                 plot.it = FALSE)$x,
      pwres_wt_theoretical_quantiles = stats::qqnorm(!!rlang::sym("wres"),
                                                     plot.it = FALSE)$x,
      iwres_wt_theoretical_quantiles = stats::qqnorm(!!rlang::sym("iwres"),
                                                     plot.it = FALSE)$x
    ) %>%
    dplyr::select(
      1:ncol(data_frame),
      pop_fit_value = !!rlang::sym("ppred"),
      ind_fit_value = !!rlang::sym("ipred"),
      pop_resid = !!rlang::sym("pres"),
      ind_resid = !!rlang::sym("ires"),
      pop_resid_quant = !!rlang::sym("pres_theoretical_quantiles"),
      ind_resid_quant = !!rlang::sym("ires_theoretical_quantiles"),
      pop_wt_resid = !!rlang::sym("wres"),
      ind_wt_resid = !!rlang::sym("iwres"),
      pop_wt_resid_quant = !!rlang::sym("pwres_wt_theoretical_quantiles"),
      ind_wt_resid_quant = !!rlang::sym("iwres_wt_theoretical_quantiles")
    )


  # Extract unique time points and number of clusters
  times <- data_frame %>%
    dplyr::pull(!!rlang::sym("time")) %>%
    unique()
  n_cluster <- data_frame %>%
    dplyr::count(!!rlang::sym("cluster")) %>%
    nrow()

  # Join times with simulated data
  simulated_data <- model_update@sim.data@datasim %>%
    dplyr::bind_cols(
      data.frame(time = rep(times, model_update@sim.data@nsim*n_cluster))
      )

  # Calculate the 2.5th and 97.5th percentiles of the predicted values
  # from each of the simulations
  simulated_data <- simulated_data %>%
    dplyr::group_by(time) %>%
    dplyr::summarize(sim_pop_pred_lb = quantile(ypred, 0.025),
                     sim_pop_pred_ub = quantile(ypred, 0.975)) %>%
    dplyr::ungroup()

  # Create a list of wide and long mixed_growth_model summary datasets
  model_summary_list <- list(
    "model_summary_wide" = model_summary_wide,
    "model_summary_long" = model_summary_long,
    "model_residual_data" = model_residual_data,
    "model_pop_ci" = simulated_data
  )

  # Return the model_summary_list
  return(model_summary_list)
}
