#' Pest Developmental Thresholds
#'
#' @format A data frame with 90 rows and 5 columns:
#' \describe{
#'   \item{pest_code}{Short abbreviation for the pest (e.g., OLFF, NOW).}
#'   \item{pest_name}{Common name of the pest.}
#'   \item{lower_thresh}{Lower developmental temperature threshold (°F).}
#'   \item{upper_thresh}{Upper developmental temperature threshold (°F).}
#'   \item{flight_interval_dd}{Cumulative degree-days between successive
#'     flight peaks, used by \code{calc_pest_phenology()} to project
#'     subsequent flight generations.}
#' }
"pest_thresholds"
