#' Summarize mixed-effects growth model object and data
#'
#' @description
#' This function is used within the \code{\link{summarize_growth_model}}
#' function to create a list object of
#' data frames based on a user's input data frame and output mixed-effects
#' growth model object from \code{\link{growth_curve_model_fit}}.
#' The list object (referred to in this package as 'growth_model_summary_list')
#' can be used to extract model predicted values, residuals, and can be
#' inputted into supporting functions from GrowthCurveME to generate plots and
#' perform model diagnostics.
#'
#'
#' @inheritParams growth_curve_model_fit
#' @param mixed_growth_model The mixed-effects model object that is created
#' using the \code{\link{growth_curve_model_fit}}
#'
#' @inherit summarize_growth_model return
#' @seealso \code{\link{growth_curve_model_fit}}
#' \code{\link{summarize_growth_model}}
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange bind_rows bind_cols bind_rows case_when filter
#' group_by mutate mutate_if rename select summarize ungroup
#' @importFrom tibble rownames_to_column tibble
#' @importFrom stats qqnorm residuals
#' @importFrom rlang sym :=
#' @importFrom stringr str_replace str_detect
#' @importFrom tidyr crossing pivot_wider
#' @importFrom stats quantile
#' @importFrom saemix compute.sres
#' @export
#'
#' @examples
#' \donttest{
#' # Load example data (exponential data)
#' data(exp_mixed_data)
#' # Fit an mixed-effects growth model to the data
#' exp_mixed_model <- growth_curve_model_fit(
#' data_frame = exp_mixed_data,
#' function_type = "exponential",
#' return_summary = FALSE)
#' # Summarize the data by creating a summary list object
#' exp_mixed_model_summary <- summarize_growth_model_mixed(
#' data_frame = exp_mixed_data,
#' mixed_growth_model = exp_mixed_model,
#' fixed_rate = TRUE,
#' function_type = "exponential",
#' time_unit = "hours")
#' model_summary_wide <- exp_mixed_model_summary[["model_summary_wide"]]
#' }
summarize_growth_model_mixed <- function(data_frame,
                                         mixed_growth_model,
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
      rate_ub = mixed_growth_model@results@conf.int[2, 6]
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
          aic = mixed_growth_model@results@aic.is,
          bic = mixed_growth_model@results@bic.is,
          loglik = mixed_growth_model@results@ll.is
        )
    } else {
      model_summary_wide <- model_summary_wide %>%
        dplyr::mutate(
          double_time_est = log(2) / !!rlang::sym("rate_est"),
          double_time_lb = log(2) / !!rlang::sym("rate_ub"),
          double_time_ub = log(2) / !!rlang::sym("rate_lb"),
          aic = mixed_growth_model@results@aic.is,
          bic = mixed_growth_model@results@bic.is,
          loglik = mixed_growth_model@results@ll.is
        )
    }

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
        double_time_est = round(!!rlang::sym("double_time_est"), 2),
        double_time_lb = round(!!rlang::sym("double_time_lb"), 2),
        double_time_ub = round(!!rlang::sym("double_time_ub"), 2),
        double_time_ci = paste(!!rlang::sym("double_time_est"),
                               " [", !!rlang::sym("double_time_lb"),
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
        "Number of clusters" = !!rlang::sym("number_clusters"),
        "Intercept [95% CI]" = !!rlang::sym("intercept_ci"),
        "Rate constant [95% CI]" = !!rlang::sym("rate_ci"),
        !!paste("Doubling time estimate (", time_unit, ") [95% CI]", sep = "")
        := !!rlang::sym("double_time_ci"),
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

    # If fixed_rate is FALSE, add the random effect correlations
    if (fixed_rate == FALSE) {
      # Prepare random-effects correlations
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
        model_type = "mixed-effects",
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
        aic = mixed_growth_model@results@aic.is,
        bic = mixed_growth_model@results@bic.is,
        loglik = mixed_growth_model@results@ll.is,
        double_time_est = log(2) / !!rlang::sym("rate_est"),
        double_time_lb = log(2) / !!rlang::sym("rate_ub"),
        double_time_ub = log(2) / !!rlang::sym("rate_lb"),
      )

      # Round and create tidy variables and convert to long dataset for
      # table figure
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
          double_time_est = round(!!rlang::sym("double_time_est"), 2),
          double_time_lb = round(!!rlang::sym("double_time_lb"), 2),
          double_time_ub = round(!!rlang::sym("double_time_ub"), 2),
          double_time_ci = paste(!!rlang::sym("double_time_est"),
                                 " [", !!rlang::sym("double_time_lb"),
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
          "Number of clusters" = !!rlang::sym("number_clusters"),
          "Lower asymptote estimate [95% CI]" = !!rlang::sym("lower_asy_ci"),
          "Upper asymptote estimate [95% CI]" = !!rlang::sym("upper_asy_ci"),
          "Inflection point estimate [95% CI]" = !!rlang::sym("inflection_ci"),
          "Rate constant estimate [95% CI]" = !!rlang::sym("rate_ci"),
          !!paste("Doubling time estimate (", time_unit, ") [95% CI]", sep = "")
          := !!rlang::sym("double_time_ci"),
          "Akaike information criterion (AIC)" = !!rlang::sym("aic"),
          "Bayesian information criterion (BIC)" = !!rlang::sym("bic"),
          "Log likelihood" = !!rlang::sym("loglik")
        )

      # Transpose data and convert to dataframe
      model_summary_long <- as.data.frame(t(model_summary_long))
      # Move rownames to a column and rename to Value
      model_summary_long <- model_summary_long %>%
        tibble::rownames_to_column(var = "Variable") %>%
        dplyr::rename("Value" = !!rlang::sym("V1"))

      # Prepare random-effects correlations
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
    dplyr::arrange(!!rlang::sym("cluster"), !!rlang::sym("time")) %>%
    dplyr::bind_cols(data.frame(wres = model_update@results@wres))

  # Used for residual diagnostics
  suppressMessages(
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
  )

  # Count number of clusters and time points
  cluster_time_count <- data_frame %>%
    dplyr::count(
      !!rlang::sym("cluster"),
      !!rlang::sym("time")
    )

  # Loop through and expand each cluster and time by their count
  cluster_time_replicates <- data.frame(
    cluster = as.character(),
    time = as.numeric(),
    obs = as.numeric()
  )
  for (a in 1:nrow(cluster_time_count)) {
    data_temp <- data.frame(
      cluster = as.character(rep(cluster_time_count[a, "cluster"],
                                 cluster_time_count[a, "n"])),
      time = as.numeric(rep(cluster_time_count[a, "time"],
                            cluster_time_count[a, "n"])),
      obs = as.numeric(seq(1, as.numeric(cluster_time_count[a, "n"]), 1))
    )

    cluster_time_replicates <- cluster_time_replicates %>%
      dplyr::bind_rows(data_temp)

    rm(data_temp)
  }
  rm(a)

  # Create cross table of unique cluster and time points by number of
  # simulations performed
  cluster_time_replicates <- cluster_time_replicates %>%
    tidyr::crossing(replicate = 1:as.numeric(model_update@sim.data@nsim)) %>%
    dplyr::arrange(
      !!rlang::sym("replicate"),
      !!rlang::sym("cluster"),
      !!rlang::sym("time")
    )

  # Join crossed data set with simulated data and calculate variables
  ci_sim_data <- data.frame(model_update@sim.data@datasim) %>%
    dplyr::bind_cols(cluster_time_replicates) %>%
    dplyr::group_by(!!rlang::sym("time")) %>%
    dplyr::summarize(
      sim_pop_pred_value = stats::quantile(!!rlang::sym("ysim"), 0.50),
      sim_pop_pred_lb = stats::quantile(!!rlang::sym("ysim"), 0.025),
      sim_pop_pred_ub = stats::quantile(!!rlang::sym("ysim"), 0.975)
    )

  # Create a list of wide and long mixed_growth_model summary datasets
  model_summary_list <- list(
    "model_summary_wide" = model_summary_wide,
    "model_summary_long" = model_summary_long,
    "model_residual_data" = model_residual_data,
    "model_sim_pred_data" = ci_sim_data
  )

  # Return the model_summary_list
  return(model_summary_list)
}
