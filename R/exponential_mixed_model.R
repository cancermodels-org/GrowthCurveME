#' Fit an exponential mixed-effects regression model
#'
#' @description
#' This function is utilized within the
#' \code{\link{growth_curve_model_fit}} function for fitting a mono-exponential
#' mixed-effects regression model to growth data utilizing the saemix package.
#' Starting values are derived from an initial least-squares model using
#' the \code{\link[minpack.lm]{nlsLM}} function.
#'
#' @param data_frame A data frame object that at minimum contains three
#' variables:
#' \itemize{
#'  \item cluster - a character type variable used to specify how observations
#'  are nested or grouped by a particular cluster. Note if using a
#'  least-squares model, please fill in all values of cluster with a single
#'  placeholder character string, do NOT leave blank.
#'  \item time - a numeric type variable used for measuring time such as
#'  minutes, hours, or days
#'  \item growth_metric - a numeric type variable used for measuring growth
#'  over time such as cell count or confluency
#' }
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
#' @param seed A numeric value specifying a seed number to reproduce the
#' random starting values sampled within the function. Defaults to NULL.
#'
#' @return Returns an exponential model object of class 'SaemixObject' when a
#' mixed-effects model is specified or a model object of class 'nls' if a
#' least-squares model is specified.
#' @seealso
#' \code{\link{growth_curve_model_fit}}
#' @importFrom magrittr %>%
#' @importFrom dplyr filter pull summarise
#' @importFrom minpack.lm nlsLM
#' @importFrom saemix saemix saemixData saemixModel
#' @importFrom stats runif nls
#' @importFrom rlang sym
#' @export
#'
#' @examples
#' \donttest{
#' # Load example data (exponential data from GrowthCurveME package)
#' data(exp_mixed_data)
#' # Fit an exponential mixed-effects growth model
#' exp_mixed_model <- growth_curve_model_fit(
#'   data_frame = exp_mixed_data,
#'   function_type = "exponential"
#' )
#' # Fit an exponential mixed-effected model using exponential_mixed_model()
#' exp_mixed_model <- exponential_mixed_model(data_frame = exp_mixed_data)
#' }
exponential_mixed_model <- function(data_frame,
                                    model_type = "mixed",
                                    fixed_rate = TRUE,
                                    num_chains = 1,
                                    seed = NULL) {
  # Calculating starting value for intercept
  start_intercept <- data_frame %>%
    dplyr::filter(!!rlang::sym("time") == min(!!rlang::sym("time"))) %>%
    dplyr::summarise(mean = mean(!!rlang::sym("growth_metric"),
                                 na.rm = TRUE)) %>%
    dplyr::pull(!!rlang::sym("mean"))
  # Fit an initial exponential model using the minpack.lm::nlsLM function
  # to derive starting values for the exponential mixed-effects model
  ls_model <- tryCatch(
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
    }
  )

  # Obtain starting values for mixed model
  if (inherits(ls_model, "nls")) {
    # Extract model estimates from ls_model to use as starting values
    start_intercept <- round(as.numeric(ls_model$m$getPars()[1]), 6)
    start_rate <- round(as.numeric(ls_model$m$getPars()[2]), 6)

    # Create summary object
    sum_object <- summary(ls_model)

    # Create sequence of other starting values
    intercept_sd <- sum_object$coefficients[1, 2] * sqrt(nrow(data_frame))
    rate_sd <- sum_object$coefficients[2, 2] * sqrt(nrow(data_frame))

    # Set a seed if applicable
    if(!is.null(seed)){
      set.seed(seed)
    }

    start_intercept_vec <- stats::runif(
      n = 10,
      min = start_intercept - (2 * intercept_sd),
      max = start_intercept + (2 * intercept_sd)
    )
    start_rate_vec <- stats::runif(
      n = 10,
      min = start_rate - (2 * rate_sd),
      max = start_rate + (2 * rate_sd)
    )

    start_intercept_vec <- c(start_intercept, start_intercept_vec)
    start_rate_vec <- c(start_rate, start_rate_vec)

    # Fit a regular nls model from stats package
    nls_model <- try(stats::nls(
      data = data_frame,
      formula = growth_metric ~ intercept * exp(rate * time),
      start = list(
        intercept = start_intercept,
        rate = start_rate
      )
    ))

  } else {
    stop(paste(
      "Initial least-squares model did not converge,",
      "function selected may not be appropriate for data"
    ))
  }

  # If model_type model is specified
  if (model_type == "mixed") {
    # Prepare SAEMIX data object
    saemix_data <- saemix::saemixData(
      name.data = data_frame,
      name.group = "cluster",
      name.predictors = "time",
      name.response = "growth_metric",
      units = list(x = "time", y = "growth_metric"),
      verbose = FALSE
    )
    # Create SAEMIX model function
    saemix_function <- function(psi, id, x) {
      time <- x[, 1]
      intercept <- psi[id, 1]
      rate <- psi[id, 2]

      ypred <- intercept * exp(rate * time)

      return(ypred)
    }
    # Set saemix NLMEG options
    NLMEG.options <- list(
      seed = 1234, displayProgress = FALSE,
      print = FALSE, save = FALSE,
      save.graphs = FALSE,
      nb.chains = num_chains
    )

    # If fixed_rate is TRUE
    if (fixed_rate == TRUE) {
      model_saemix <- "No"
      count <- 1
      while (!inherits(model_saemix, "SaemixObject") &
        count <= length(start_intercept_vec)) {
        # Set model structure
        saemix_model_object <- saemix::saemixModel(
          model = saemix_function,
          description = "SAEMIX Exponential Model",
          psi0 = c(
            intercept = start_intercept_vec[count],
            rate = start_rate_vec[count]
          ),
          covariance.model = matrix(
            c(
              1, 0,
              0, 0
            ),
            ncol = 2, byrow = TRUE
          ),
          transform.par = c(0, 0),
          verbose = FALSE
        )
        # Fit mixed-effects model
        model_saemix <- tryCatch(
          expr = {
            withCallingHandlers(
              saemix::saemix(
                model = saemix_model_object,
                data = saemix_data,
                control = NLMEG.options
              ),
              warning = function(w) {
                invokeRestart("muffleWarning")
              }
            )
          },
          error = function(e) {
            error_message <- paste("Caution an error occured: ", e)
            message(error_message)
            return("No")
          }
        )
        count <- count + 1
      }
    } else {
      model_saemix <- "No"
      count <- 1
      while (!inherits(model_saemix, "SaemixObject") &
        count <= length(start_intercept_vec)) {
        # Set model structure
        saemix_model_object <- saemix::saemixModel(
          model = saemix_function,
          description = "SAEMIX Exponential Model",
          psi0 = c(
            intercept = start_intercept_vec[count],
            rate = start_rate_vec[count]
          ),
          covariance.model = matrix(
            c(
              1, 1,
              1, 1
            ),
            ncol = 2, byrow = TRUE
          ),
          transform.par = c(0, 0),
          verbose = FALSE
        )
        # Fit mixed-effects model
        model_saemix <- tryCatch(
          expr = {
            withCallingHandlers(
              saemix::saemix(
                model = saemix_model_object,
                data = saemix_data,
                control = NLMEG.options
              ),
              warning = function(w) {
                invokeRestart("muffleWarning")
              }
            )
          },
          error = function(e) {
            error_message <- paste("Caution an error occured: ", e)
            message(error_message)
            return("No")
          }
        )
        count <- count + 1
      }
    }
    if (inherits(model_saemix, "SaemixObject")) {
      # Return the mixed-model
      return(model_saemix)
    } else {
      stop(paste(
        "Mixed-effects model did not converge,",
        "function selected may not be appropriate for data"
      ))
    }
  } else {
    # Check that nls_model is nls
    if(inherits(nls_model, "nls")){
      # Return the ls model
      return(nls_model)
    }else{
      stop(paste(
        "Conversion of nls model from minpack.lm nlsLM to stats nls model",
        "failed."
      ))
    }
  }
}
