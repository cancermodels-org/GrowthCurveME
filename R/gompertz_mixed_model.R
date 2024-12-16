#' Fit a Gompertz mixed-effects regression model
#'
#' @description
#' This function is utilized within the
#' \code{\link{growth_curve_model_fit}} function for fitting a Gompertz
#' mixed-effects regression model to growth data utilizing the saemix package.
#' Starting values are derived from an initial least-squares model using
#' the \code{\link[minpack.lm]{nlsLM}} function.
#'
#'
#' @inheritParams exponential_mixed_model
#'
#' @return Returns a Gompertz model object of class 'SaemixObject' if a
#' mixed-effects model is specified or a model object of class 'nls' if a
#' least-squares model is specified.
#' @seealso
#' \code{\link{growth_curve_model_fit}}
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate pull summarise
#' @importFrom minpack.lm nlsLM
#' @importFrom saemix saemix saemixData saemixModel
#' @importFrom stats as.formula runif nls
#' @importFrom rlang sym
#' @export
#'
#' @examples
#' \donttest{
#' # Load example data (Gompertz data from GrowthCurveME package)
#' data(gomp_mixed_data)
#' # Fit a Gompertz mixed-effects growth model
#' gomp_mixed_model <- growth_curve_model_fit(
#'   data_frame = gomp_mixed_data,
#'   function_type = "gompertz"
#' )
#' # Fit a Gompertz mixed-effected model using gompertz_mixed_model()
#' gomp_mixed_model <- gompertz_mixed_model(data_frame = gomp_mixed_data)
#' }
gompertz_mixed_model <- function(data_frame,
                                 model_type = "mixed",
                                 fixed_rate = TRUE,
                                 num_chains = 1,
                                 seed = NULL) {
  # Calculate initial starting values
  start_lower_asy <- data_frame %>%
    dplyr::filter(!!rlang::sym("time") == min(!!rlang::sym("time"))) %>%
    dplyr::summarise(mean = mean(!!rlang::sym("growth_metric"),
                                 na.rm = TRUE)) %>%
    dplyr::pull(!!rlang::sym("mean"))
  start_upper_asy <- max(data_frame$growth_metric, na.rm = TRUE)
  mid_point <- start_upper_asy - start_lower_asy
  start_inflection <- data_frame %>%
    dplyr::mutate(
      diff_growth_metric = abs(!!rlang::sym("growth_metric") - mid_point)
    ) %>%
    dplyr::filter(
      !!rlang::sym("diff_growth_metric") ==
        min(!!rlang::sym("diff_growth_metric"))
    ) %>%
    dplyr::pull(!!rlang::sym("time"))
  # Fit an initial least-squares Gompertz model to calculate starting values
  gompertz_formula <- as.formula(
    paste0(
      "growth_metric ~ lower_asy + (upper_asy - lower_asy)*",
      "exp(-exp(-rate*(time-inflection)))"
    )
  )
  ls_model <- tryCatch(
    expr = {
      withCallingHandlers(
        minpack.lm::nlsLM(gompertz_formula,
          data = data_frame,
          start = list(
            lower_asy = start_lower_asy,
            upper_asy = start_upper_asy,
            rate = 0.05,
            inflection = start_inflection
          )
        ),
        warning = function(w) {
          invokeRestart("muffleWarning")
        }
      )
    },
    error = function(e) {
      error_message <- paste("Caution an error occurred: ", e)
      message(error_message)
    }
  )

  # Obtain starting values for mixed model
  if (inherits(ls_model, "nls")) {
    # Extract model estimates from ls_model to use as starting values
    start_lower_asy <- round(as.numeric(ls_model$m$getPars()[1]), 3)
    start_upper_asy <- round(as.numeric(ls_model$m$getPars()[2]), 3)
    start_rate <- round(as.numeric(ls_model$m$getPars()[3]), 6)
    start_inflection <- round(as.numeric(ls_model$m$getPars()[4]), 3)

    # Create summary object
    sum_object <- summary(ls_model)

    # Create sequence of other starting values
    lower_asy_sd <- sum_object$coefficients[1, 2] * sqrt(nrow(data_frame))
    upper_asy_sd <- sum_object$coefficients[2, 2] * sqrt(nrow(data_frame))
    rate_sd <- sum_object$coefficients[3, 2] * sqrt(nrow(data_frame))
    inflection_sd <- sum_object$coefficients[4, 2] * sqrt(nrow(data_frame))

    # Set a seed if applicable
    if(!is.null(seed)){
      set.seed(seed)
    }

    start_lower_asy_vec <- stats::runif(
      n = 10,
      min = start_lower_asy - (2 * lower_asy_sd),
      max = start_lower_asy + (2 * lower_asy_sd)
    )
    start_upper_asy_vec <- stats::runif(
      n = 10,
      min = start_upper_asy - (2 * upper_asy_sd),
      max = start_upper_asy + (2 * upper_asy_sd)
    )
    start_rate_vec <- stats::runif(
      n = 10,
      min = start_rate - (2 * rate_sd),
      max = start_rate + (2 * rate_sd)
    )
    start_inflection_vec <- stats::runif(
      n = 10,
      min = start_inflection - (2 * inflection_sd),
      max = start_inflection + (2 * inflection_sd)
    )

    start_lower_asy_vec <- c(start_lower_asy, start_lower_asy_vec)
    start_upper_asy_vec <- c(start_upper_asy, start_upper_asy_vec)
    start_rate_vec <- c(start_rate, start_rate_vec)
    start_inflection_vec <- c(start_inflection, start_inflection_vec)

    # Fit a regular nls model from stats package
    nls_model <- try(stats::nls(
      data = data_frame,
      formula = gompertz_formula,
      start = list(
        lower_asy = start_lower_asy,
        upper_asy = start_upper_asy,
        rate = start_rate,
        inflection = start_inflection
      )
    ))

  } else {
    stop(paste(
      "Initial least-squares model did not converge,",
      "function selected may not be appropriate for data"
    ))
  }

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
      lower_asy <- psi[id, 1]
      upper_asy <- psi[id, 2]
      rate <- psi[id, 3]
      inflection <- psi[id, 4]

      ypred <- lower_asy + (upper_asy - lower_asy) * exp(-exp(-rate * (time - inflection)))

      return(ypred)
    }
    # Set NLMEG options
    NLMEG.options <- list(
      seed = 1234, displayProgress = FALSE,
      print = FALSE, save = FALSE,
      save.graphs = FALSE,
      nb.chains = num_chains
    )
    # If fixed_rate == TRUE
    if (fixed_rate == TRUE) {
      model_saemix <- "No"
      count <- 1
      while (!inherits(model_saemix, "SaemixObject") &
        count <= length(start_lower_asy_vec)) {
        # Set model structure
        saemix_model_object <- saemix::saemixModel(
          model = saemix_function,
          description = "SAEMIX Gompertz Model",
          psi0 = c(
            lower_asy = start_lower_asy_vec[count],
            upper_asy = start_upper_asy_vec[count],
            rate = start_rate_vec[count],
            inflection = start_inflection_vec[count]
          ),
          covariance.model = matrix(c(
            1, 1, 0, 1,
            1, 1, 0, 1,
            0, 0, 0, 0,
            1, 1, 0, 1
          ), ncol = 4, byrow = TRUE),
          transform.par = c(0, 0, 0, 0),
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
            error_message <- paste("Caution an error occurred: ", e)
            message(error_message)
            return("No")
          }
        )
        count <- count + 1
      }
      # If fixed_rate is FALSE
    } else {
      model_saemix <- "No"
      count <- 1
      while (!inherits(model_saemix, "SaemixObject") &
        count <= length(start_lower_asy_vec)) {
        # Set model structure
        saemix_model_object <- saemix::saemixModel(
          model = saemix_function,
          description = "SAEMIX Gompertz Model",
          psi0 = c(
            lower_asy = start_lower_asy_vec[count],
            upper_asy = start_upper_asy_vec[count],
            rate = start_rate_vec[count],
            inflection = start_inflection_vec[count]
          ),
          covariance.model = matrix(c(
            1, 1, 1, 1,
            1, 1, 1, 1,
            1, 1, 1, 1,
            1, 1, 1, 1
          ), ncol = 4, byrow = TRUE),
          transform.par = c(0, 0, 0, 0),
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
            error_message <- paste("Caution an error occurred: ", e)
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
