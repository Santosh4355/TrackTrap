make_cimis_csv <- function(
    dates,
    tmax = 85,
    tmin = 55,
    date_fmt = "%m/%d/%Y"
) {
  dates <- as.Date(dates)
  
  if (length(dates) == 0L) {
    stop("`dates` must contain at least one date.", call. = FALSE)
  }
  
  if (length(tmax) == 1L) {
    tmax <- rep(tmax, length(dates))
  }
  
  if (length(tmin) == 1L) {
    tmin <- rep(tmin, length(dates))
  }
  
  if (length(tmax) != length(dates)) {
    stop("`tmax` must have length 1 or length(dates).", call. = FALSE)
  }
  
  if (length(tmin) != length(dates)) {
    stop("`tmin` must have length 1 or length(dates).", call. = FALSE)
  }
  
  cimis_data <- data.frame(
    Date = format(dates, date_fmt),
    Max.Air.Temp..F. = tmax,
    Min.Air.Temp..F. = tmin,
    check.names = FALSE
  )
  
  path <- tempfile(fileext = ".csv")
  
  utils::write.csv(
    cimis_data,
    file = path,
    row.names = FALSE
  )
  
  path
}