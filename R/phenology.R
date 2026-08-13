#' Fetch Open-Meteo Data
#'
#' @param lat Latitude.
#' @param lon Longitude.
#' @param start_date Start date, "YYYY-MM-DD".
#' @param end_date End date, "YYYY-MM-DD". Defaults to today.
#' @return A data frame with Date, tmax, tmin (Fahrenheit).
#' @export
fetch_open_meteo <- function(lat, lon, start_date,
                             end_date = as.character(Sys.Date())) {
  
  response <- httr::GET(
    "https://archive-api.open-meteo.com/v1/archive",
    query = list(
      latitude = lat,
      longitude = lon,
      start_date = start_date,
      end_date = end_date,
      daily = "temperature_2m_max,temperature_2m_min",
      temperature_unit = "fahrenheit",
      timezone = "America/Los_Angeles"
    )
  )
  if (httr::http_error(response)) stop("Failed to fetch data from Open-Meteo.")
  
  parsed <- httr::content(response, as = "parsed", type = "application/json")
  if (is.null(parsed$daily) || is.null(parsed$daily$time)) {
    stop("Open-Meteo response missing expected 'daily' data. Check lat/lon and date range.")
  }
  
  as_num <- function(x) sapply(x, function(v) if (is.null(v)) NA_real_ else as.numeric(v))
  
  weather_data <- data.frame(
    Date = as.Date(unlist(parsed$daily$time)),
    tmax = as_num(parsed$daily$temperature_2m_max),
    tmin = as_num(parsed$daily$temperature_2m_min)
  )
  message(sprintf("Open-Meteo fetch complete: %d days retrieved.", nrow(weather_data)))
  weather_data
}

#' Calculate Pest Phenology
#'
#' @param trap_data Data frame with \code{date} and \code{trap_counts}.
#' @param pest Pest code from \code{pest_thresholds} (e.g. "OLFF", "CM").
#'   Leave \code{NULL} and provide \code{custom_lower}/\code{custom_upper}
#'   for a pest not in the database.
#' @param lat,lon Trap location. Required for "open_meteo" and "daymet".
#' @param weather_source Either "open_meteo", "daymet", "cimis_csv".
#' @param cimis_csv_path Path to a CIMIS daily report (Fahrenheit),
#'   required when \code{weather_source = "cimis_csv"}.
#' @param custom_lower,custom_upper Override or provide the developmental
#'   thresholds in Fahrenheit.
#' @return \code{trap_data} merged to daily weather, with
#'   \code{cumulative_dd} (running seasonal degree-days) and
#'   \code{cumulative_dd_from_biofix} (nullified at first trap catch).
#'   Uses \code{lat}, \code{lon}, and \code{year} as attributes for
#'   \code{plot_trap_phenology()}.
#' @export
calc_pest_phenology <- function(trap_data, pest = NULL, lat = NULL, lon = NULL,
                                weather_source = "open_meteo", cimis_csv_path = NULL,
                                custom_lower = NULL, custom_upper = NULL) {
  
  if (!is.null(pest)) {
    pest_info <- TrackTrap::pest_thresholds[
      TrackTrap::pest_thresholds$pest_code == toupper(pest), ]
    if (nrow(pest_info) == 0) {
      stop("Pest code not found in database. Use custom_lower and custom_upper.")
    }
    lower_thresh <- if (is.null(custom_lower)) pest_info$lower_thresh else custom_lower
    upper_thresh <- if (is.null(custom_upper)) pest_info$upper_thresh else custom_upper
    message(sprintf("Using thresholds for %s: Lower = %s F, Upper = %s F",
                    pest_info$pest_name, lower_thresh, upper_thresh))
  } else {
    if (is.null(custom_lower) || is.null(custom_upper)) {
      stop("You must provide either a 'pest' code (e.g., 'OLFF', 'NOW', 'CM') OR both 'custom_lower' and 'custom_upper'.")
    }
    lower_thresh <- custom_lower
    upper_thresh <- custom_upper
    message(sprintf("Using custom thresholds: Lower = %s F, Upper = %s F",
                    lower_thresh, upper_thresh))
  }
  
  trap_data$date <- as.Date(trap_data$date)
  fetch_start <- as.character(min(trap_data$date, na.rm = TRUE))
  fetch_end   <- as.character(max(trap_data$date, na.rm = TRUE))
  data_year   <- format(min(trap_data$date, na.rm = TRUE), "%Y")
  
  weather_data <- switch(
    weather_source,
    
    "open_meteo" = {
      if (is.null(lat) || is.null(lon)) stop("Latitude and longitude are required for 'open_meteo'.")
      message("Fetching historical and live weather from Open-Meteo...")
      fetch_open_meteo(lat, lon, fetch_start, fetch_end)
    },
    
    "daymet" = {
      if (is.null(lat) || is.null(lon)) stop("Latitude and longitude are required for 'daymet'.")
      if (as.numeric(substr(fetch_end, 1, 4)) >= as.numeric(format(Sys.Date(), "%Y"))) {
        stop("Daymet only supports data up to the previous calendar year. Use 'open_meteo' for the current year.")
      }
      message("Fetching historical weather from NASA Daymet...")
      raw <- daymetr::download_daymet(
        lat = lat, lon = lon,
        start = as.numeric(substr(fetch_start, 1, 4)),
        end = as.numeric(substr(fetch_end, 1, 4)),
        internal = TRUE, silent = TRUE
      )
      out <- data.frame(
        Date = as.Date(paste(raw$data$year, raw$data$yday, sep = "-"), "%Y-%j"),
        tmax = (raw$data$tmax..deg.c. * 9 / 5) + 32,
        tmin = (raw$data$tmin..deg.c. * 9 / 5) + 32
      )
      message(sprintf("Daymet fetch complete: %d days retrieved for (%.4f, %.4f).",
                      nrow(out), lat, lon))
      out
    },
    
    "cimis_csv" = {
      if (is.null(cimis_csv_path)) stop("Must provide cimis_csv_path when using 'cimis_csv'.")
      if (!is.null(lat) || !is.null(lon)) {
        message("Note: lat/lon are ignored for 'cimis_csv'; station location comes from the CSV itself.")
      }
      message("Loading local CIMIS CSV data...")
      raw <- utils::read.csv(cimis_csv_path, stringsAsFactors = FALSE)
      
      required_cols <- c("Date", "Max.Air.Temp..F.", "Min.Air.Temp..F.")
      missing_cols <- setdiff(required_cols, names(raw))
      if (length(missing_cols) > 0) {
        stop(sprintf(
          "cimis_csv_path is missing required column(s): %s. Ensure the report was exported in Fahrenheit as a Daily Report. Columns found: %s",
          paste(missing_cols, collapse = ", "), paste(names(raw), collapse = ", ")
        ))
      }
      
      parsed_dates <- as.Date(raw$Date, format = "%m/%d/%Y")
      if (all(is.na(parsed_dates))) {
        stop("cimis_csv_path Date column could not be parsed as MM/DD/YYYY. Check that the file wasn't reformatted by a spreadsheet program.")
      }
      
      out <- data.frame(
        Date = parsed_dates,
        tmax = suppressWarnings(as.numeric(raw$Max.Air.Temp..F.)),
        tmin = suppressWarnings(as.numeric(raw$Min.Air.Temp..F.))
      )
      message(sprintf("CIMIS CSV load complete: %d days read from %s.",
                      nrow(out), basename(cimis_csv_path)))
      out
    },
    
    stop("Invalid weather_source. Use 'open_meteo', 'daymet', or 'cimis_csv'.")
  )
  
  weather_data$avg_temp <- (weather_data$tmax + weather_data$tmin) / 2
  weather_data$avg_temp <- pmin(weather_data$avg_temp, upper_thresh)
  weather_data$DD <- pmax(weather_data$avg_temp - lower_thresh, 0)
  
  n_na_dd <- sum(is.na(weather_data$DD))
  if (n_na_dd > 0) {
    message(sprintf("Note: %d day(s) had missing temperature data and were treated as 0 degree-days.", n_na_dd))
    weather_data$DD[is.na(weather_data$DD)] <- 0
  }
  weather_data$cumulative_dd <- cumsum(weather_data$DD)
  
  final_df <- merge(trap_data, weather_data, by.x = "date", by.y = "Date", all.x = TRUE)
  
  biofix_date <- suppressWarnings(min(final_df$date[final_df$trap_counts > 0], na.rm = TRUE))
  if (is.infinite(biofix_date) || is.na(biofix_date)) {
    stop("No positive trap counts found; cannot set biofix.")
  }
  biofix_dd <- final_df$cumulative_dd[final_df$date == biofix_date][1]
  final_df$cumulative_dd_from_biofix <- final_df$cumulative_dd - biofix_dd
  
  attr(final_df, "lat")  <- lat
  attr(final_df, "lon")  <- lon
  attr(final_df, "year") <- data_year
  
  final_df
}

#' Plot Trap Phenology with Flight Markers
#'
#' @param pheno_data Output of \code{calc_pest_phenology()}.
#' @param pest Pest code, used to explore the display name and flight
#'   interval. Optional.
#' @param year,lat,lon Override the plot title's year/coordinates. If
#'   omitted, retrieved from \code{pheno_data}'s attributes.
#' @export
plot_trap_phenology <- function(pheno_data, pest = NULL, year = NULL,
                                lat = NULL, lon = NULL) {
  
  pheno_data$cumulative_dd_from_biofix <- as.numeric(pheno_data$cumulative_dd_from_biofix)
  
  if (is.null(year)) {
    year <- attr(pheno_data, "year")
    if (is.null(year) && "date" %in% names(pheno_data)) {
      year <- format(min(pheno_data$date, na.rm = TRUE), "%Y")
    }
  }
  if (is.null(lat)) lat <- attr(pheno_data, "lat")
  if (is.null(lon)) lon <- attr(pheno_data, "lon")
  
  pest_info <- NULL
  if (!is.null(pest)) {
    pest_info <- TrackTrap::pest_thresholds[
      TrackTrap::pest_thresholds$pest_code == toupper(pest), ]
  }
  
  year_suffix <- if (!is.null(year)) sprintf(" (%s)", year) else ""
  location_suffix <- if (!is.null(lat) && !is.null(lon)) {
    sprintf(" @ (%.4f, %.4f)", as.numeric(lat), as.numeric(lon))
  } else ""
  
  base_title <- if (!is.null(pest_info) && nrow(pest_info) == 1) {
    sprintf("%s (%s) Phenology", pest_info$pest_name, pest_info$pest_code)
  } else if (!is.null(pest)) {
    sprintf("%s Phenology", toupper(pest))
  } else {
    "Pest Phenology"
  }
  
  p <- ggplot2::ggplot(pheno_data, ggplot2::aes(x = cumulative_dd_from_biofix, y = trap_counts)) +
    ggplot2::geom_line(color = "darkred", linewidth = 1.2) +
    ggplot2::geom_point(color = "red", size = 3) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste0(base_title, year_suffix, location_suffix),
      subtitle = "Trap Catch vs. Accumulated Degree-Days",
      x = "Accumulated Degree-Days from Biofix",
      y = "Trap Catch Count"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      axis.title = ggplot2::element_text(face = "bold")
    )
  
  if (!is.null(pest_info) && nrow(pest_info) == 1 && !is.na(pest_info$flight_interval_dd)) {
    interval <- pest_info$flight_interval_dd
    flights  <- interval * 0:3
    labels   <- c("1st Flight\n(Biofix)", "2nd Flight", "3rd Flight", "4th Flight")
    y_pos    <- max(pheno_data$trap_counts, na.rm = TRUE) * seq(0.95, 0.65, length.out = 4)
    
    for (i in seq_along(flights)) {
      p <- p +
        ggplot2::geom_vline(xintercept = flights[i], linetype = "dashed", color = "blue", linewidth = 0.8) +
        ggplot2::annotate("text", x = flights[i] + interval * 0.03, y = y_pos[i],
                          label = labels[i], hjust = 0, color = "blue", size = 3.5)
    }
  } else if (!is.null(pest)) {
    message("Note: No generation interval defined in database for this pest. Flight lines skipped.")
  }
  
  p
}
