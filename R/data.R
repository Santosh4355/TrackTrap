#' Pest temperature thresholds and thermal reference values
#'
#' Reference parameters and source information for 90 pest records.
#' Missing parameters are stored as NA. Inclusion does not imply that
#' a complete or independently validated phenology model is available.
#'
#' @format A data frame with 90 rows and 10 variables:
#' \describe{
#'   \item{pest_code}{Unique pest identifier.}
#'   \item{pest_name}{Common name.}
#'   \item{scientific_name}{Scientific name.}
#'   \item{lower_thresh}{Lower temperature threshold in degrees Fahrenheit.}
#'   \item{upper_thresh}{Upper temperature threshold in degrees Fahrenheit,
#'     where available.}
#'   \item{thermal_requirement_dd}{Thermal reference value in Fahrenheit
#'     degree-days for the stated endpoint; not necessarily a flight interval.}
#'   \item{lower_source}{Reference supporting the lower threshold.}
#'   \item{upper_source}{Reference supporting the upper threshold.}
#'   \item{thermal_requirement_source}{Reference for the thermal value.}
#'   \item{thermal_requirement_endpoint}{Developmental or phenological
#'     endpoint associated with the thermal value.}
#' }
#' @details
#' Parameter sources and model compatibility require assessment before
#' application. Seasonal event thresholds and developmental requirements
#' are not interchangeable.
"pest_thresholds"
