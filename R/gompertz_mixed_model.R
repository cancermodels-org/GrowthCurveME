#' Fit a Gompertz mixed-effects regression model
#'
#' @description
#' 'gompertz_mixed_model()' is a function utilized with the \code{\link{growth_curve_model_fit}} function for fitting a Gompertz mixed-effects regression model to growth data utilizing the \code{\link[nlme]{nlme}} function.
#' Starting values are derived from an initial least-squares model using the \code{\link[minpack.lm]{nlsLM}} function.
#'
#'
#' @inheritParams growth_curve_model_fit
#'
#' @return Returns a Gompertz model object of class 'nlme' when a mixed-effects model is specified or a model object of class 'nls' if a least-squares model is specified.
#' @seealso \code{\link{growth_curve_model_fit}}
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate pull summarise
#' @importFrom minpack.lm nlsLM
#' @importFrom nlme nlme nlmeControl
#' @importFrom stats as.formula na.omit
#' @export
#'
#' @examples
#' # Load example data (Gompertz data from GrowthCurveME package)
#' data(gomp_mixed_data)
#' # Fit a Gompertz mixed-effects growth model to the data using the main growth_curve_model_fit() function
#' gomp_mixed_model <- growth_curve_model_fit(data_frame = gomp_mixed_data, function_type = "gompertz")
#' # Fit a Gompertz mixed-effected model using the gompertz_mixed_model() function
#' gomp_mixed_model <- gompertz_mixed_model(data_frame = gomp_mixed_data)
gompertz_mixed_model <- function(data_frame,
                                 model_type = "mixed",
                                 fixed_rate = TRUE) {
  # Calculate initial starting values
  start_lower_asy <- data_frame %>%
    dplyr::filter(time == min(time)) %>%
    dplyr::summarise(mean = mean(growth_metric), na.rm = TRUE) %>%
    dplyr::pull(mean)
  start_upper_asy <- max(data_frame$growth_metric, na.rm = TRUE)
  mid_point <- start_upper_asy - start_lower_asy
  start_inflection <- data_frame %>%
    dplyr::mutate(diff_growth_metric = abs(growth_metric - mid_point)) %>%
    dplyr::filter(diff_growth_metric == min(diff_growth_metric)) %>%
    dplyr::pull(time)
  # Fit an initial least squares sigmoidal model to calculate better starting values
  gompertz_formula <- as.formula("growth_metric ~ lower_asy + (upper_asy - lower_asy)*exp(-exp(-rate*(time-inflection)))")
  initial_gompertz_model <- tryCatch(
    expr = {
      withCallingHandlers(
        minpack.lm::nlsLM(gompertz_formula,
          start = list(
            lower_asy = start_lower_asy,
            upper_asy = start_upper_asy,
            rate = 0.05,
            inflection = start_inflection
          ),
          data = data_frame
        ),
        warning = function(w) {
          invokeRestart("muffleWarning")
        }
      )
    },
    error = function(e) {
      error_message <- paste("Caution an error occured: ", e)
      message(error_message)
      stop("Initial non-linear least squares model could not be fit due to error, please inspect data or change function type")
    }
  )
  if (model_type == "mixed") {
    # Extract model estimates from initial_sigmoid_model to use as starting values for mixed model
    start_lower_asy <- round(as.numeric(initial_gompertz_model$m$getPars()[1]), 3)
    start_upper_asy <- round(as.numeric(initial_gompertz_model$m$getPars()[2]), 3)
    start_rate <- round(as.numeric(initial_gompertz_model$m$getPars()[3]), 6)
    start_inflection <- round(as.numeric(initial_gompertz_model$m$getPars()[4]), 3)
    # If fixed_rate == TRUE
    if (fixed_rate == TRUE) {
      # Fit non-linear (gompertz) mixed effects model with random lower asymptote, upper asymptote, and inflection point
      gompertz_model <- tryCatch(
        expr = {
          withCallingHandlers(
            nlme::nlme(gompertz_formula,
              start = c(
                lower_asy = start_lower_asy,
                upper_asy = start_upper_asy,
                rate = start_rate,
                inflection = start_inflection
              ),
              method = "REML",
              fixed = lower_asy + upper_asy + rate + inflection ~ 1,
              random = lower_asy + upper_asy + inflection ~ 1,
              groups = ~cluster,
              na.action = na.omit,
              control = nlme::nlmeControl(maxIter = 5000),
              data = data_frame
            ),
            warning = function(w) {
              invokeRestart("muffleWarning")
            }
          )
        },
        error = function(e) {
          error_message <- paste("Caution an error occured: ", e)
          message(error_message)
          stop("Initial fit of gompertz mixed model failed, current model specification may be in-appropriate for provided data, please inspect data or change function type")
        }
      )
      # If fixed_rate == FALSE
    } else {
      # Fit non-linear (gompertz) mixed effects model with random lower asymptote, upper asymptote, and inflection point
      gompertz_model <- tryCatch(
        expr = {
          withCallingHandlers(
            nlme::nlme(gompertz_formula,
              start = c(
                lower_asy = start_lower_asy,
                upper_asy = start_upper_asy,
                rate = start_rate,
                inflection = start_inflection
              ),
              method = "REML",
              fixed = lower_asy + upper_asy + rate + inflection ~ 1,
              random = lower_asy + upper_asy + rate + inflection ~ 1,
              groups = ~cluster,
              na.action = na.omit,
              control = nlme::nlmeControl(maxIter = 5000),
              data = data_frame
            ),
            warning = function(w) {
              invokeRestart("muffleWarning")
            }
          )
        },
        error = function(e) {
          error_message <- paste("Caution an error occured: ", e)
          message(error_message)
          stop("Initial fit of gompertz mixed model failed, current model specification may be in-appropriate for provided data, please inspect data or change function type")
        }
      )
    }

    return(gompertz_model)
  } else {
    return(initial_gompertz_model)
  }
}
