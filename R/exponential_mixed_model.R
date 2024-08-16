#' Fit an exponential mixed-effects regression model
#'
#' @description
#' 'exponential_mixed_model()' is a function utilized with the \code{\link{growth_curve_model_fit}} function for fitting a mono-exponential mixed-effects regression model to growth data utilizing the \code{\link[nlme]{nlme}} function.
#' Starting values are derived from an initial least-squares model using the \code{\link[minpack.lm]{nlsLM}} function.
#'
#'
#' @inheritParams growth_curve_model_fit
#'
#' @return Returns an exponential model object of class 'nlme' when a mixed-effects model is specified or a model object of class 'nls' if a least-squares model is specified.
#' @seealso \link{growth_curve_model_fit}
#' @importFrom magrittr %>%
#' @importFrom dplyr filter pull summarise
#' @importFrom minpack.lm nlsLM
#' @importFrom nlme nlme nlmeControl
#' @importFrom stats na.omit
#' @export
#'
#' @examples
#' # Load example data (exponential data from GrowthCurveME package)
#' data(exp_mixed_data)
#' # Fit an exponential mixed-effects growth model to the data using the main growth_curve_model_fit() function
#' exp_mixed_model <- growth_curve_model_fit(data_frame = exp_mixed_data, function_type = "exponential")
#' # Fit an exponential mixed-effected model using the exponential_mixed_model() function
#' exp_mixed_model <- exponential_mixed_model(data_frame = exp_mixed_data)
exponential_mixed_model <- function(data_frame,
                                    model_type = "mixed",
                                    fixed_rate = TRUE) {
  # Calculating starting value for intercept
  start_intercept <- data_frame %>%
    dplyr::filter(time == min(time)) %>%
    dplyr::summarise(mean = mean(growth_metric, na.rm = TRUE)) %>%
    dplyr::pull(mean)
  # Fit an initial exponential model using the minpack.lm::nlsLM to get starting values for exponential mixed effects model
  initial_exp_model <- tryCatch(
    expr = {
      withCallingHandlers(
        minpack.lm::nlsLM(growth_metric ~ intercept * exp(rate * time),
          start = list(
            intercept = start_intercept,
            rate = 0
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
  # If model_type model is specified
  if (model_type == "mixed") {
    # Extract model estimates from initial_exp_model to use as starting values
    start_intercept <- round(as.numeric(initial_exp_model$m$getPars()[1]), 6)
    start_rate <- round(as.numeric(initial_exp_model$m$getPars()[2]), 6)
    # If fixed_rate is TRUE
    if (fixed_rate == TRUE) {
      # Fit non-linear (exponential) mixed model with random intercepts and fixed rates to the data
      exp_model <- tryCatch(
        expr = {
          withCallingHandlers(
            nlme::nlme(growth_metric ~ intercept * exp(rate * time),
              method = "REML",
              data = data_frame,
              fixed = intercept + rate ~ 1,
              random = intercept ~ 1,
              groups = ~cluster,
              start = c(
                intercept = start_intercept,
                rate = start_rate
              ),
              na.action = na.omit,
              control = nlme::nlmeControl(maxIter = 5000)
            ),
            warning = function(w) {
              invokeRestart("muffleWarning")
            }
          )
        },
        error = function(e) {
          error_message <- paste("Caution an error occured: ", e)
          message(error_message)
          stop("Initial fit of exponential mixed model failed, current model specification may be in-appropriate for provided data, please inspect data or change function type")
        }
      )
      # If fixed_rate is FALSE
    } else {
      # Fit non-linear (exponential) mixed model with random intercepts and random rates to the data
      exp_model <- tryCatch(
        expr = {
          withCallingHandlers(
            nlme::nlme(growth_metric ~ intercept * exp(rate * time),
              method = "REML",
              data = data_frame,
              fixed = intercept + rate ~ 1,
              random = intercept + rate ~ 1,
              groups = ~cluster,
              start = c(
                intercept = start_intercept,
                rate = start_rate
              ),
              na.action = na.omit,
              control = nlme::nlmeControl(maxIter = 5000)
            ),
            warning = function(w) {
              invokeRestart("muffleWarning")
            }
          )
        },
        error = function(e) {
          error_message <- paste("Caution an error occured: ", e)
          message(error_message)
          stop("Initial fit of exponential mixed model failed, current model specification may be in-appropriate for provided data, please inspect data, change fixed_rate to TRUE, or change function type")
        }
      )
    }
    # Return the exponential model_type model
    return(exp_model)
    # If model_type model is not requested
  } else {
    # Return the exponential nls model
    return(initial_exp_model)
  }
}
