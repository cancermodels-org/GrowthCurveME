#' Sample exponential growth dataset
#'
#' A dataset containing the minimum required variables needed to input data
#' into the GrowthCurveME package functions
#'
#' @format A data frame with 240 rows and 3 variables:
#' \describe{
#'   \item{cluster}{A character type variable used to specify the clustering
#'   of values by a particular metric. Note even when selecting a least-squares,
#'   do not leave this variable NA or empty. Instead fill in these values with
#'   a single repetitive dummy variable (e.g., '1') for the package to
#'   run properly}
#'   \item{time}{A numeric type variable for any measurement in time such as
#'   minutes, hours, or days}
#'   \item{growth_metric}{A numeric type variable for measuring growth such as
#'   confluency or cell count}
#' }
#'
#' @source {Created through simulation to serve as an example}
#'
#' @examples
#' data(exp_mixed_data)
"exp_mixed_data"


#' Sample logistic growth dataset
#'
#' A dataset containing the minimum required variables needed to input data
#' into the GrowthCurveME package functions
#'
#' @format A data frame with 800 rows and 3 variables:
#' \describe{
#'   \item{cluster}{A character type variable used to specify the clustering
#'   of values by a particular metric. Note even when selecting a least-squares,
#'   do not leave this variable NA or empty. Instead fill in these values with
#'   a single repetitive dummy variable (e.g., '1') for the package to
#'   run properly}
#'   \item{time}{A numeric type variable for any measurement in time such as
#'   minutes, hours, or days}
#'   \item{growth_metric}{A numeric type variable for measuring growth such as
#'   confluency or cell count}
#' }
#'
#' @source Created through simulation to serve as an example
#'
#' @examples
#' data(log_mixed_data)
"log_mixed_data"


#' Sample Gompertz growth dataset
#'
#' A dataset containing the minimum required variables needed to input data
#' into the GrowthCurveME package functions
#'
#' @format A data frame with 210 rows and 3 variables:
#' \describe{
#'   \item{cluster}{A character type variable used to specify the clustering
#'   of values by a particular metric. Note even when selecting a least-squares,
#'   do not leave this variable NA or empty. Instead fill in these values with
#'   a single repetitive dummy variable (e.g., '1') for the package to
#'   run properly}
#'   \item{time}{A numeric type variable for any measurement in time such as
#'   minutes, hours, or days}
#'   \item{growth_metric}{A numeric type variable for measuring growth such
#'   as confluency or cell count}
#' }
#'
#' @source Created through simulation to serve as an example
#'
#' @examples
#' data(gomp_mixed_data)
"gomp_mixed_data"


#' Sample linear growth dataset
#'
#' A dataset containing the minimum required variables needed to input data
#' into the GrowthCurveME package functions
#'
#' @format A data frame with 110 rows and 3 variables:
#' \describe{
#'   \item{cluster}{A character type variable used to specify the clustering
#'   of values by a particular metric. Note even when selecting a least-squares,
#'   do not leave this variable NA or empty. Instead fill in these values with
#'   a single repetitive dummy variable (e.g., '1') for the package to
#'   run properly}
#'   \item{time}{A numeric type variable for any measurement in time such as
#'   minutes, hours, or days}
#'   \item{growth_metric}{A numeric type variable for measuring growth such as
#'   confluency or cell count}
#' }
#'
#' @source Created through simulation to serve as an example
#'
#' @examples
#' data(lin_mixed_data)
"lin_mixed_data"





