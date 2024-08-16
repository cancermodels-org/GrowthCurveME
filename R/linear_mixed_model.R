#' Fit a linear mixed-effects regression model
#'
#' @description
#' 'linear_mixed_model()' is a function utilized with the \code{\link{growth_curve_model_fit}} function for fitting a linear mixed-effects regression model to growth data utilizing the \code{\link[nlme]{lme}} function.
#'
#'
#' @inheritParams growth_curve_model_fit
#'
#' @return Returns a linear model object of class 'nlme' when a mixed-effects model is specified or a model object of class 'nls' if a least-squares model is specified.
#' @seealso \code{\link{growth_curve_model_fit}}
#' @importFrom nlme nlme
#' @importFrom stats lm
#' @importFrom stats na.omit
#' @export
#'
#' @examples
#' # Load example data (linear data from GrowthCurveME package)
#' data(lin_mixed_data)
#' # Fit a linear mixed-effects growth model to the data using the main growth_curve_model_fit() function
#' lin_mixed_model <- growth_curve_model_fit(data_frame = lin_mixed_data, function_type = "linear")
#' # Fit an exponential mixed-effected model using the exponential_mixed_model() function
#' lin_mixed_model <- linear_mixed_model(data_frame = lin_mixed_data)
linear_mixed_model <- function(data_frame,
                               model_type = "mixed",
                               fixed_rate = TRUE) {
  if (model_type == "mixed") {
    # If fixed_rate is TRUE
    if (fixed_rate == TRUE) {
      # Fit a linear mixed effects model with random intercepts and fixed slopes (growth_metric is log transformed in order to calculate doubling time)
      linear_model <- tryCatch(
        expr = {
          withCallingHandlers(
            nlme::lme(growth_metric ~ time,
              method = "REML",
              data = data_frame,
              random = ~ 1 | cluster,
              na.action = na.omit
            ),
            warning = function(w) {
              invokeRestart("muffleWarning")
            }
          )
        },
        error = function(e) {
          error_message <- paste("Caution an error occured: ", e)
          message(error_message)
          stop("Initial fit of linear mixed model failed, current model specification may be in-appropriate for provided data, please inspect data or change function type")
        }
      )
      # If fixed_rate is FALSE
    } else {
      # Fit a linear mixed effects model with random intercepts and random slopes (growth_metric is log transformed in order to calculate doubling time)
      linear_model <- tryCatch(
        expr = {
          withCallingHandlers(
            nlme::lme(growth_metric ~ time,
              method = "REML",
              data = data_frame,
              random = ~ time | cluster,
              na.action = na.omit
            ),
            warning = function(w) {
              invokeRestart("muffleWarning")
            }
          )
        },
        error = function(e) {
          error_message <- paste("Caution an error occured: ", e)
          message(error_message)
          stop("Initial fit of linear mixed model failed, current model specification may be in-appropriate for provided data, please inspect data, change fixed_rate to TRUE, or change function type")
        }
      )
    }

    # Return the linear mixed effects model object
    return(linear_model)
  } else {
    # Fit a linear model with ordinary least squares (fixed_rate no longer applies)
    ols_linear_model <- lm(growth_metric ~ time,
      data = data_frame
    )
    return(ols_linear_model)
  }
}
