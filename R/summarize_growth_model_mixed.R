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
#' @importFrom dplyr arrange case_when filter mutate mutate_if rename select
#' @importFrom tibble rownames_to_column tibble
#' @importFrom nlme intervals VarCorr
#' @importFrom stats qqnorm residuals
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
    inherits(mixed_growth_model, c("lme", "nlme"))
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
  sum_object <- summary(mixed_growth_model)
  # Extract coefficients from mixed_growth_model into respective data frames
  fixed_effects <- nlme::intervals(mixed_growth_model, which = "fixed")
  # If regression mixed_growth_model function_type is exponential or linear
  if (function_type == "exponential") {
    # If the rate is fixed
    if (fixed_rate == TRUE) {
      # Create wide data set to store variables and components within it
      model_summary_wide <- tibble::tibble(
        model_function = function_type,
        model_type = "mixed-effects",
        number_observations = sum_object$dims$N,
        number_clusters = as.numeric(sum_object$dims$ngrps[1]),
        intercept = mixed_growth_model$coefficients$fixed[1],
        intercept_lb = fixed_effects$fixed[1, 1],
        intercept_ub = fixed_effects$fixed[1, 3],
        intercept_df = sum_object$tTable[1, 3],
        intercept_t_statistic = sum_object$tTable[1, 4],
        intercept_p_value = sum_object$tTable[1, 5],
        rate = mixed_growth_model$coefficients$fixed[2],
        rate_lb = fixed_effects$fixed[2, 1],
        rate_ub = fixed_effects$fixed[2, 3],
        rate_df = sum_object$tTable[1, 3],
        rate_t_statistic = sum_object$tTable[2, 4],
        rate_p_value = sum_object$tTable[2, 5],
        double_time_hours = log(2) / rate,
        double_time_hours_lb = log(2) / rate_ub,
        double_time_hours_ub = log(2) / rate_lb,
        AIC = sum_object$AIC,
        BIC = sum_object$BIC,
        loglik = sum_object$logLik,
        random_effect_intercept_variance = as.numeric(nlme::VarCorr(mixed_growth_model)[1, 1]),
        random_effect_residual_variance = as.numeric(nlme::VarCorr(mixed_growth_model)[2, 1]),
        rate_intercept_cor = sum_object$corFixed[2, 1]
      )
      # Create fixed effects correlation table
      model_fixed_correlations <- as.data.frame(sum_object$corFixed) %>%
        tibble::rownames_to_column(var = "variable")

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
          loglik = round(loglik, 2),
          random_effect_intercept_variance = round(random_effect_intercept_variance, 2),
          random_effect_residual_variance = round(random_effect_residual_variance, 2),
          rate_intercept_cor = round(rate_intercept_cor, 3)
        ) %>%
        dplyr::mutate_if(is.numeric, as.character) %>%
        dplyr::select(
          "Model function" = model_function,
          "Model type" = model_type,
          "Number of observations" = number_observations,
          "Number of clusters" = number_clusters,
          "Intercept [95% CI]" = intercept_ci,
          "Intercept p-value" = intercept_p_value,
          "Rate constant [95% CI]" = rate_ci,
          "Rate constant p-value" = rate_p_value,
          "Doubling time estimate [95% CI]" = double_time_ci,
          "Akaike information criterion (AIC)" = AIC,
          "Bayesian information criterion (BIC)" = BIC,
          "Log likelihood" = loglik,
          "Random effect intercept variance" = random_effect_intercept_variance,
          "Random effect residual variance" = random_effect_residual_variance,
          "Rate-intercept correlation" = rate_intercept_cor
        )
      # Transpose and convert to a dataframe
      model_summary_long <- as.data.frame(t(model_summary_long))
      # Convert rownames to a column and rename to Value
      model_summary_long <- model_summary_long %>%
        tibble::rownames_to_column(var = "Variable") %>%
        dplyr::rename("Value" = "V1")
      # If the rate is random
    } else {
      # Create wide data set to store variables and components within it
      model_summary_wide <- tibble::tibble(
        model_function = function_type,
        model_type = "mixed-effects",
        number_observations = sum_object$dims$N,
        number_clusters = as.numeric(sum_object$dims$ngrps[1]),
        intercept = mixed_growth_model$coefficients$fixed[1],
        intercept_lb = fixed_effects$fixed[1, 1],
        intercept_ub = fixed_effects$fixed[1, 3],
        intercept_df = sum_object$tTable[1, 3],
        intercept_t_statistic = sum_object$tTable[1, 4],
        intercept_p_value = sum_object$tTable[1, 5],
        rate = mixed_growth_model$coefficients$fixed[2],
        rate_lb = fixed_effects$fixed[2, 1],
        rate_ub = fixed_effects$fixed[2, 3],
        rate_df = sum_object$tTable[1, 3],
        rate_t_statistic = sum_object$tTable[2, 4],
        rate_p_value = sum_object$tTable[2, 5],
        double_time_hours = log(2) / rate,
        double_time_hours_lb = log(2) / rate_ub,
        double_time_hours_ub = log(2) / rate_lb,
        AIC = sum_object$AIC,
        BIC = sum_object$BIC,
        loglik = sum_object$logLik,
        random_effect_intercept_variance = as.numeric(nlme::VarCorr(mixed_growth_model)[1, 1]),
        random_effect_rate_variance = as.numeric(nlme::VarCorr(mixed_growth_model)[2, 1]),
        random_effect_residual_variance = as.numeric(nlme::VarCorr(mixed_growth_model)[3, 1]),
        rate_intercept_cor = sum_object$corFixed[2, 1]
      )

      # Create fixed effects correlation table
      model_fixed_correlations <- as.data.frame(sum_object$corFixed) %>%
        tibble::rownames_to_column(var = "variable")

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
          loglik = round(loglik, 2),
          random_effect_intercept_variance = round(random_effect_intercept_variance, 2),
          random_effect_residual_variance = round(random_effect_residual_variance, 2),
          random_effect_residual_variance = round(random_effect_residual_variance, 2),
          rate_intercept_cor = round(rate_intercept_cor, 3)
        ) %>%
        dplyr::mutate_if(is.numeric, as.character) %>%
        dplyr::select(
          "Model function" = model_function,
          "Model type" = model_type,
          "Number of observations" = number_observations,
          "Number of clusters" = number_clusters,
          "Intercept estimate [95% CI]" = intercept_ci,
          "Intercept p-value" = intercept_p_value,
          "Rate constant estimate [95% CI]" = rate_ci,
          "Rate constant p-value" = rate_p_value,
          "Doubling time estimate [95% CI]" = double_time_ci,
          "Akaike information criterion (AIC)" = AIC,
          "Bayesian information criterion (BIC)" = BIC,
          "Log likelihood" = loglik,
          "Random effect intercept variance" = random_effect_intercept_variance,
          "Random effect rate variance" = random_effect_rate_variance,
          "Random effect residual variance" = random_effect_residual_variance,
          "Rate-intercept correlation" = rate_intercept_cor
        )
      # Transpose data and convert to dataframe
      model_summary_long <- as.data.frame(t(model_summary_long))

      model_summary_long <- model_summary_long %>%
        tibble::rownames_to_column(var = "Variable") %>%
        dplyr::rename("Value" = "V1")
    }
  }

  # If regression mixed_growth_model function_type is linear
  if (function_type == "linear") {
    # If the rate is fixed
    if (fixed_rate == TRUE) {
      # Create wide data set to store variables and components within it
      model_summary_wide <- tibble::tibble(
        model_function = function_type,
        model_type = "mixed-effects",
        number_observations = sum_object$dims$N,
        number_clusters = as.numeric(sum_object$dims$ngrps[1]),
        intercept = mixed_growth_model$coefficients$fixed[1],
        intercept_lb = fixed_effects$fixed[1, 1],
        intercept_ub = fixed_effects$fixed[1, 3],
        intercept_df = sum_object$tTable[1, 3],
        intercept_t_statistic = sum_object$tTable[1, 4],
        intercept_p_value = sum_object$tTable[1, 5],
        rate = mixed_growth_model$coefficients$fixed[2],
        rate_lb = fixed_effects$fixed[2, 1],
        rate_ub = fixed_effects$fixed[2, 3],
        rate_df = sum_object$tTable[1, 3],
        rate_t_statistic = sum_object$tTable[2, 4],
        rate_p_value = sum_object$tTable[2, 5],
        double_time_hours = 2 / rate,
        double_time_hours_lb = 2 / rate_ub,
        double_time_hours_ub = 2 / rate_lb,
        AIC = sum_object$AIC,
        BIC = sum_object$BIC,
        loglik = sum_object$logLik,
        random_effect_intercept_variance = as.numeric(nlme::VarCorr(mixed_growth_model)[1, 1]),
        random_effect_residual_variance = as.numeric(nlme::VarCorr(mixed_growth_model)[2, 1]),
        rate_intercept_cor = sum_object$corFixed[2, 1]
      )
      # Create fixed effects correlation table
      model_fixed_correlations <- as.data.frame(sum_object$corFixed) %>%
        tibble::rownames_to_column(var = "variable")

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
          loglik = round(loglik, 2),
          random_effect_intercept_variance = round(random_effect_intercept_variance, 2),
          random_effect_residual_variance = round(random_effect_residual_variance, 2),
          rate_intercept_cor = round(rate_intercept_cor, 3)
        ) %>%
        dplyr::mutate_if(is.numeric, as.character) %>%
        dplyr::select(
          "Model function" = model_function,
          "Model type" = model_type,
          "Number of observations" = number_observations,
          "Number of clusters" = number_clusters,
          "Intercept [95% CI]" = intercept_ci,
          "Intercept p-value" = intercept_p_value,
          "Rate constant [95% CI]" = rate_ci,
          "Rate constant p-value" = rate_p_value,
          "Doubling time estimate [95% CI]" = double_time_ci,
          "Akaike information criterion (AIC)" = AIC,
          "Bayesian information criterion (BIC)" = BIC,
          "Log likelihood" = loglik,
          "Random effect intercept variance" = random_effect_intercept_variance,
          "Random effect residual variance" = random_effect_residual_variance,
          "Rate-intercept correlation" = rate_intercept_cor
        )
      # Transpose and convert to a dataframe
      model_summary_long <- as.data.frame(t(model_summary_long))
      # Convert rownames to a column and rename to Value
      model_summary_long <- model_summary_long %>%
        tibble::rownames_to_column(var = "Variable") %>%
        dplyr::rename("Value" = "V1")
      # If the rate is random
    } else {
      # Create wide data set to store variables and components within it
      model_summary_wide <- tibble::tibble(
        model_function = function_type,
        model_type = "mixed-effects",
        number_observations = sum_object$dims$N,
        number_clusters = as.numeric(sum_object$dims$ngrps[1]),
        intercept = mixed_growth_model$coefficients$fixed[1],
        intercept_lb = fixed_effects$fixed[1, 1],
        intercept_ub = fixed_effects$fixed[1, 3],
        intercept_df = sum_object$tTable[1, 3],
        intercept_t_statistic = sum_object$tTable[1, 4],
        intercept_p_value = sum_object$tTable[1, 5],
        rate = mixed_growth_model$coefficients$fixed[2],
        rate_lb = fixed_effects$fixed[2, 1],
        rate_ub = fixed_effects$fixed[2, 3],
        rate_df = sum_object$tTable[1, 3],
        rate_t_statistic = sum_object$tTable[2, 4],
        rate_p_value = sum_object$tTable[2, 5],
        double_time_hours = 2 / rate,
        double_time_hours_lb = 2 / rate_ub,
        double_time_hours_ub = 2 / rate_lb,
        AIC = sum_object$AIC,
        BIC = sum_object$BIC,
        loglik = sum_object$logLik,
        random_effect_intercept_variance = as.numeric(nlme::VarCorr(mixed_growth_model)[1, 1]),
        random_effect_rate_variance = as.numeric(nlme::VarCorr(mixed_growth_model)[2, 1]),
        random_effect_residual_variance = as.numeric(nlme::VarCorr(mixed_growth_model)[3, 1]),
        rate_intercept_cor = sum_object$corFixed[2, 1]
      )

      # Create fixed effects correlation table
      model_fixed_correlations <- as.data.frame(sum_object$corFixed) %>%
        tibble::rownames_to_column(var = "variable")

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
          loglik = round(loglik, 2),
          random_effect_intercept_variance = round(random_effect_intercept_variance, 2),
          random_effect_residual_variance = round(random_effect_residual_variance, 2),
          random_effect_residual_variance = round(random_effect_residual_variance, 2),
          rate_intercept_cor = round(rate_intercept_cor, 3)
        ) %>%
        dplyr::mutate_if(is.numeric, as.character) %>%
        dplyr::select(
          "Model function" = model_function,
          "Model type" = model_type,
          "Number of observations" = number_observations,
          "Number of clusters" = number_clusters,
          "Intercept estimate [95% CI]" = intercept_ci,
          "Intercept p-value" = intercept_p_value,
          "Rate constant estimate [95% CI]" = rate_ci,
          "Rate constant p-value" = rate_p_value,
          "Doubling time estimate [95% CI]" = double_time_ci,
          "Akaike information criterion (AIC)" = AIC,
          "Bayesian information criterion (BIC)" = BIC,
          "Log likelihood" = loglik,
          "Random effect intercept variance" = random_effect_intercept_variance,
          "Random effect rate variance" = random_effect_rate_variance,
          "Random effect residual variance" = random_effect_residual_variance,
          "Rate-intercept correlation" = rate_intercept_cor
        )
      # Transpose data and convert to dataframe
      model_summary_long <- as.data.frame(t(model_summary_long))

      model_summary_long <- model_summary_long %>%
        tibble::rownames_to_column(var = "Variable") %>%
        dplyr::rename("Value" = "V1")
    }
  }

  # If mixed_growth_model function_type is logistic or Gompertz
  if (function_type == "logistic" | function_type == "gompertz") {
    # If the rate is fixed
    if (fixed_rate == TRUE) {
      # Create wide data set to store variables and components within it
      model_summary_wide <- tibble::tibble(
        model_function = function_type,
        model_type = "mixed-effects",
        number_observations = sum_object$dims$N,
        number_clusters = as.numeric(sum_object$dims$ngrps[1]),
        lower_asy = mixed_growth_model$coefficients$fixed[1],
        lower_asy_lb = fixed_effects$fixed[1, 1],
        lower_asy_ub = fixed_effects$fixed[1, 3],
        lower_asy_df = sum_object$tTable[1, 3],
        lower_asy_t_statistic = sum_object$tTable[1, 4],
        lower_asy_p_value = sum_object$tTable[1, 5],
        upper_asy = mixed_growth_model$coefficients$fixed[2],
        upper_asy_lb = fixed_effects$fixed[2, 1],
        upper_asy_ub = fixed_effects$fixed[2, 3],
        upper_asy_df = sum_object$tTable[2, 3],
        upper_asy_t_statistic = sum_object$tTable[2, 4],
        upper_asy_p_value = sum_object$tTable[2, 5],
        rate = mixed_growth_model$coefficients$fixed[3],
        rate_lb = fixed_effects$fixed[3, 1],
        rate_ub = fixed_effects$fixed[3, 3],
        rate_df = sum_object$tTable[3, 3],
        rate_t_statistic = sum_object$tTable[3, 4],
        rate_p_value = sum_object$tTable[3, 5],
        inflection = mixed_growth_model$coefficients$fixed[4],
        inflection_lb = fixed_effects$fixed[4, 1],
        inflection_ub = fixed_effects$fixed[4, 3],
        inflection_df = sum_object$tTable[4, 3],
        inflection_t_statistic = sum_object$tTable[4, 4],
        inflection_p_value = sum_object$tTable[4, 5],
        double_time_hours = log(2) / rate,
        double_time_hours_lb = log(2) / rate_ub,
        double_time_hours_ub = log(2) / rate_lb,
        AIC = sum_object$AIC,
        BIC = sum_object$BIC,
        loglik = sum_object$logLik,
        random_effect_lower_asy_variance = as.numeric(nlme::VarCorr(mixed_growth_model)[1, 1]),
        random_effect_upper_asy_variance = as.numeric(nlme::VarCorr(mixed_growth_model)[2, 1]),
        random_effect_inflection_variance = as.numeric(nlme::VarCorr(mixed_growth_model)[3, 1]),
        random_effect_residual_variance = as.numeric(nlme::VarCorr(mixed_growth_model)[4, 1])
      )
      # Create fixed effects correlation table
      model_fixed_correlations <- as.data.frame(sum_object$corFixed) %>%
        tibble::rownames_to_column(var = "variable")

      # Round and create tidy variables for convert to long dataset for table figure
      model_summary_long <- model_summary_wide %>%
        dplyr::mutate(
          lower_asy = round(lower_asy, 2),
          lower_asy_lb = round(lower_asy_lb, 2),
          lower_asy_ub = round(lower_asy_ub, 2),
          lower_asy_ci = paste(lower_asy, " [", lower_asy_lb, ",", lower_asy_ub, "]",
            sep = ""
          ),
          lower_asy_df = sum_object$tTable[1, 3],
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
          upper_asy_df = sum_object$tTable[1, 3],
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
          inflection_df = sum_object$tTable[1, 3],
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
          loglik = round(loglik, 2),
          random_effect_lower_asy_variance = round(random_effect_lower_asy_variance, 2),
          random_effect_upper_asy_variance = round(random_effect_upper_asy_variance, 2),
          random_effect_inflection_variance = round(random_effect_inflection_variance, 2),
          random_effect_residual_variance = round(random_effect_residual_variance, 2)
        ) %>%
        dplyr::mutate_if(is.numeric, as.character) %>%
        dplyr::select(
          "Model function" = model_function,
          "Model type" = model_type,
          "Number of observations" = number_observations,
          "Number of clusters" = number_clusters,
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
          "Bayesian information criterion (BIC)" = BIC,
          "Log likelihood" = loglik,
          "Random effect lower asymptote variance" = random_effect_lower_asy_variance,
          "Random effect upper asymptote variance" = random_effect_upper_asy_variance,
          "Random effect inflection point variance" = random_effect_inflection_variance,
          "Random effect residual variance" = random_effect_residual_variance
        )
      # Transpose data and convert to dataframe
      model_summary_long <- as.data.frame(t(model_summary_long))
      # Move rownames to a column and rename to Value
      model_summary_long <- model_summary_long %>%
        tibble::rownames_to_column(var = "Variable") %>%
        dplyr::rename("Value" = "V1")
      # If the rate is random
    } else {
      # Create wide data set to store variables and components within it
      model_summary_wide <- tibble::tibble(
        model_function = function_type,
        model_type = "mixed-effects",
        number_observations = sum_object$dims$N,
        number_clusters = as.numeric(sum_object$dims$ngrps[1]),
        lower_asy = mixed_growth_model$coefficients$fixed[1],
        lower_asy_lb = fixed_effects$fixed[1, 1],
        lower_asy_ub = fixed_effects$fixed[1, 3],
        lower_asy_df = sum_object$tTable[1, 3],
        lower_asy_t_statistic = sum_object$tTable[1, 4],
        lower_asy_p_value = sum_object$tTable[1, 5],
        upper_asy = mixed_growth_model$coefficients$fixed[2],
        upper_asy_lb = fixed_effects$fixed[2, 1],
        upper_asy_ub = fixed_effects$fixed[2, 3],
        upper_asy_df = sum_object$tTable[2, 3],
        upper_asy_t_statistic = sum_object$tTable[2, 4],
        upper_asy_p_value = sum_object$tTable[2, 5],
        rate = mixed_growth_model$coefficients$fixed[3],
        rate_lb = fixed_effects$fixed[3, 1],
        rate_ub = fixed_effects$fixed[3, 3],
        rate_df = sum_object$tTable[3, 3],
        rate_t_statistic = sum_object$tTable[3, 4],
        rate_p_value = sum_object$tTable[3, 5],
        inflection = mixed_growth_model$coefficients$fixed[4],
        inflection_lb = fixed_effects$fixed[4, 1],
        inflection_ub = fixed_effects$fixed[4, 3],
        inflection_df = sum_object$tTable[4, 3],
        inflection_t_statistic = sum_object$tTable[4, 4],
        inflection_p_value = sum_object$tTable[4, 5],
        double_time_hours = log(2) / rate,
        double_time_hours_lb = log(2) / rate_ub,
        double_time_hours_ub = log(2) / rate_lb,
        AIC = sum_object$AIC,
        BIC = sum_object$BIC,
        loglik = sum_object$logLik,
        random_effect_lower_asy_variance = as.numeric(nlme::VarCorr(mixed_growth_model)[1, 1]),
        random_effect_upper_asy_variance = as.numeric(nlme::VarCorr(mixed_growth_model)[2, 1]),
        random_effect_rate_variance = as.numeric(nlme::VarCorr(mixed_growth_model)[3, 1]),
        random_effect_inflection_variance = as.numeric(nlme::VarCorr(mixed_growth_model)[4, 1]),
        random_effect_residual_variance = as.numeric(nlme::VarCorr(mixed_growth_model)[5, 1])
      )
      # Create fixed effects correlation table
      model_fixed_correlations <- as.data.frame(sum_object$corFixed) %>%
        tibble::rownames_to_column(var = "variable")
      # Round and create tidy variables for convert to long dataset for table figure
      model_summary_long <- model_summary_wide %>%
        dplyr::mutate(
          lower_asy = round(lower_asy, 2),
          lower_asy_lb = round(lower_asy_lb, 2),
          lower_asy_ub = round(lower_asy_ub, 2),
          lower_asy_ci = paste(lower_asy, " [", lower_asy_lb, ",", lower_asy_ub, "]",
            sep = ""
          ),
          lower_asy_df = sum_object$tTable[1, 3],
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
          upper_asy_df = sum_object$tTable[1, 3],
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
          inflection_df = sum_object$tTable[1, 3],
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
          loglik = round(loglik, 2),
          random_effect_lower_asy_variance = round(random_effect_lower_asy_variance, 2),
          random_effect_upper_asy_variance = round(random_effect_upper_asy_variance, 2),
          random_effect_rate_variance = round(random_effect_rate_variance, 2),
          random_effect_inflection_variance = round(random_effect_inflection_variance, 2),
          random_effect_residual_variance = round(random_effect_residual_variance, 2)
        ) %>%
        dplyr::mutate_if(is.numeric, as.character) %>%
        dplyr::select(
          "Model function" = model_function,
          "Model type" = model_type,
          "Number of observations" = number_observations,
          "Number of clusters" = number_clusters,
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
          "Bayesian information criterion (BIC)" = BIC,
          "Log likelihood" = loglik,
          "Random effect lower asymptote variance" = random_effect_lower_asy_variance,
          "Random effect upper asymptote variance" = random_effect_upper_asy_variance,
          "Random effect rate variance" = random_effect_rate_variance,
          "Random effect inflection point variance" = random_effect_inflection_variance,
          "Random effect residual variance" = random_effect_residual_variance
        )
      # Transpose the data and convert to dataframee
      model_summary_long <- as.data.frame(t(model_summary_long))
      # Move rownames to a column and rename to Value
      model_summary_long <- model_summary_long %>%
        tibble::rownames_to_column(var = "Variable") %>%
        dplyr::rename("Value" = "V1")
    }
  }
  # Extract original data used to fit mixed_growth_model
  model_residual_data <- data_frame
  # Extract residuals and create empty variable columns
  model_residual_data <- model_residual_data %>%
    dplyr::mutate(
      fitted_values_fixed = mixed_growth_model$fitted[, 1],
      fitted_values_random = mixed_growth_model$fitted[, 2],
      marginal_raw_residuals = stats::residuals(mixed_growth_model, level = 0),
      marginal_standardized_residuals = stats::residuals(mixed_growth_model, function_type = "normalized", level = 0),
      conditional_raw_residuals = stats::residuals(mixed_growth_model),
      conditional_standardized_residuals = stats::residuals(mixed_growth_model, function_type = "normalized"),
      marginal_standardized_theoretical_quantiles =
        stats::qqnorm(marginal_standardized_residuals, plot.it = FALSE)$x,
      conditional_standardized_theoretical_quantiles =
        stats::qqnorm(conditional_standardized_residuals, plot.it = FALSE)$x
    )
  # Create a list of wide and long mixed_growth_model summary datasets
  model_summary_list <- list(
    "model_summary_wide" = model_summary_wide,
    "model_summary_long" = model_summary_long,
    "model_residual_data" = model_residual_data,
    "model_fixed_corr" = model_fixed_correlations
  )
  # Return the model_summary_list
  return(model_summary_list)
}
