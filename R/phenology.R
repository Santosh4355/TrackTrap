parse_trap_date <- function(x, arg = "date") {
  x_chr <- trimws(as.character(x))
  parsed <- as.Date(x_chr, format = "%m/%d/%Y")
  if (anyNA(parsed) && !all(is.na(x_chr))) {
    bad <- unique(x_chr[is.na(parsed) & !is.na(x_chr)])
    stop(sprintf("Could not parse %s: %s. Dates must be in M/D/YYYY format (e.g., 3/9/2026).",
                 arg, paste(bad, collapse = ", ")), call. = FALSE)
  }
  parsed
}

#' Fetch Historical and Real-Time Weather Data from Open-Meteo
#'
#' Retrieves daily maximum and minimum temperature data for a given location
#' and date range from the Open-Meteo historical weather archive API.
#'
#' @param lat Numeric. Latitude of the location.
#' @param lon Numeric. Longitude of the location.
#' @param start_date Character. Start date in `"YYYY-MM-DD"` format.
#' @param end_date Character. End date in `"YYYY-MM-DD"` format. Defaults to
#'   the current system date.
#'
#' @return A data frame with columns `Date`, `tmax`, and `tmin` (degrees
#'   Fahrenheit).
#' @export
fetch_open_meteo <- function(lat, lon, start_date, end_date = as.character(Sys.Date())) {
  response <- httr::GET(
    "https://archive-api.open-meteo.com/v1/archive",
    query = list(latitude = lat, longitude = lon, start_date = start_date, end_date = end_date,
                 daily = "temperature_2m_max,temperature_2m_min",
                 temperature_unit = "fahrenheit", timezone = "America/Los_Angeles")
  )
  if (httr::http_error(response)) stop("Failed to fetch data from Open-Meteo.")
  
  parsed <- httr::content(response, as = "parsed", type = "application/json")
  if (is.null(parsed$daily) || is.null(parsed$daily$time)) {
    stop("Open-Meteo response missing expected 'daily' data. Check lat/lon and date range.")
  }
  
  as_num <- function(x) vapply(x, function(v) if (is.null(v)) NA_real_ else as.numeric(v), numeric(1))
  
  weather_data <- data.frame(
    Date = as.Date(unlist(parsed$daily$time)),
    tmax = as_num(parsed$daily$temperature_2m_max),
    tmin = as_num(parsed$daily$temperature_2m_min)
  )
  message(sprintf("Open-Meteo fetch complete: %d days retrieved.", nrow(weather_data)))
  weather_data
}

#' Calculate Pest Phenology from Trap Counts and Weather Data
#'
#' @param trap_data A data frame with columns `date` (character, `M/D/YYYY`
#'   format) and `trap_counts` (numeric).
#' @param pest Character. A pest code (e.g., `"OLFF"`) checked against the
#'   built-in `pest_thresholds` database. Optional if both `custom_lower`
#'   and `custom_upper` are provided.
#' @param lat Numeric. Latitude of the trap location. Required when
#'   `weather_source` is `"open_meteo"` or `"daymet"`.
#' @param lon Numeric. Longitude of the trap location. Required when
#'   `weather_source` is `"open_meteo"` or `"daymet"`. 
#' @param weather_source Character. One of `"open_meteo"`, `"daymet"`, or
#'   `"cimis_csv"`. Defaults to `"open_meteo"`.
#' @param cimis_csv_path Character. File path to a manually downloaded CIMIS
#'   daily CSV report. Required when `weather_source = "cimis_csv"`.
#' @param custom_lower Numeric. Custom lower developmental threshold (°F).
#'   Overrides the database value if `pest` is also provided.
#' @param custom_upper Numeric. Custom upper developmental threshold (°F).
#'   Pass `NA` explicitly (or leave the database value as `NA`) to calculate
#'   degree-days without an upper cutoff.
#' @param custom_flight_interval Numeric. Custom degree-day interval between
#'   pest generations/flights, used for plotting flight lines.
#' @param custom_pest_label Character. Custom label used in plot titles when
#'   `pest` is not found in the built-in database.
#'
#' @return A data frame combining `trap_data` with merged weather and
#'   degree-day columns, including `cumulative_dd` and
#'   `cumulative_dd_from_biofix`.
#' @export
calc_pest_phenology <- function(trap_data, pest = NULL, lat = NULL, lon = NULL,
                                weather_source = "open_meteo", cimis_csv_path = NULL,
                                custom_lower = NULL, custom_upper = NULL,
                                custom_flight_interval = NULL, custom_pest_label = NULL) {
  
  pest_label <- NULL
  override_note <- NULL
  
  if (!is.null(pest)) {
    pest_info <- TrackTrap::pest_thresholds[TrackTrap::pest_thresholds$pest_code == toupper(pest), ]
    n_match <- nrow(pest_info)
    pest_found <- n_match == 1
    
    if (n_match > 1) {
      stop(sprintf("Pest code '%s' matched %d rows in the reference database (expected 0 or 1).",
                   toupper(pest), n_match))
    }
    if (!pest_found && (is.null(custom_lower) || is.null(custom_upper))) {
      stop("Pest code not found in database. Provide both custom_lower and custom_upper (and optionally custom_pest_label) for an unlisted pest.")
    }
    if (pest_found && (!is.null(custom_lower) || !is.null(custom_upper))) {
      override_note <- "user override replacing database threshold"
    }
    
    lower_thresh <- if (!is.null(custom_lower)) custom_lower else pest_info$lower_thresh
    upper_thresh <- if (!is.null(custom_upper)) custom_upper else pest_info$upper_thresh
    flight_interval <- if (!is.null(custom_flight_interval)) custom_flight_interval
    else if (pest_found) pest_info$flight_interval_dd else NA
    pest_label <- if (pest_found) sprintf("%s (%s)", pest_info$pest_name, pest_info$pest_code)
    else if (!is.null(custom_pest_label)) sprintf("%s (%s)", custom_pest_label, toupper(pest))
    else toupper(pest)
    
    if (is.na(upper_thresh)) {
      message(sprintf(
        "Using thresholds for %s: Lower = %s F, Upper = not defined (calculating without an upper cutoff)%s",
        pest_label, lower_thresh,
        if (!is.null(override_note)) paste0(" [", override_note, "]") else ""))
    } else {
      message(sprintf("Using thresholds for %s: Lower = %s F, Upper = %s F%s",
                      pest_label, lower_thresh, upper_thresh,
                      if (!is.null(override_note)) paste0(" [", override_note, "]") else ""))
    }
  } else {
    if (is.null(custom_lower) || is.null(custom_upper)) {
      stop("You must provide either a 'pest' code (e.g., 'OLFF', 'NOW', 'CM') OR both 'custom_lower' and 'custom_upper'.")
    }
    lower_thresh <- custom_lower
    upper_thresh <- custom_upper
    flight_interval <- custom_flight_interval
    pest_label <- if (!is.null(custom_pest_label)) custom_pest_label else "Custom Pest"
    
    if (is.na(upper_thresh)) {
      message(sprintf("Using custom thresholds: Lower = %s F, Upper = not defined (calculating without an upper cutoff)", lower_thresh))
    } else {
      message(sprintf("Using custom thresholds: Lower = %s F, Upper = %s F", lower_thresh, upper_thresh))
    }
  }
  
  trap_data$date <- parse_trap_date(trap_data$date, arg = "trap_data$date")
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
      raw <- daymetr::download_daymet(lat = lat, lon = lon,
                                      start = as.numeric(substr(fetch_start, 1, 4)),
                                      end = as.numeric(substr(fetch_end, 1, 4)),
                                      internal = TRUE, silent = TRUE)
      out <- data.frame(
        Date = as.Date(paste(raw$data$year, raw$data$yday, sep = "-"), "%Y-%j"),
        tmax = (raw$data$tmax..deg.c. * 9 / 5) + 32,
        tmin = (raw$data$tmin..deg.c. * 9 / 5) + 32
      )
      message(sprintf("Daymet fetch complete: %d days retrieved for (%.4f, %.4f).", nrow(out), lat, lon))
      out
    },
    
    "cimis_csv" = {
      if (is.null(cimis_csv_path)) stop("Must provide cimis_csv_path when using 'cimis_csv'.")
      if (!is.null(lat) || !is.null(lon)) {
        message("Note: lat/lon are ignored for 'cimis_csv'; TrackTrap does not extract station location from the CIMIS CSV. Provide lat/lon directly to plot_trap_phenology() if a location title is needed.")
      }
      message("Loading local CIMIS CSV data...")
      raw <- utils::read.csv(cimis_csv_path, stringsAsFactors = FALSE)
      
      required_cols <- c("Date", "Max.Air.Temp..F.", "Min.Air.Temp..F.")
      missing_cols <- setdiff(required_cols, names(raw))
      if (length(missing_cols) > 0) {
        stop(sprintf("cimis_csv_path is missing required column(s): %s. Columns found: %s",
                     paste(missing_cols, collapse = ", "), paste(names(raw), collapse = ", ")))
      }
      
      out <- data.frame(
        Date = parse_trap_date(raw$Date, arg = "cimis_csv_path Date column"),
        tmax = suppressWarnings(as.numeric(raw$Max.Air.Temp..F.)),
        tmin = suppressWarnings(as.numeric(raw$Min.Air.Temp..F.))
      )
      message(sprintf("CIMIS CSV load complete: %d days read from %s.", nrow(out), basename(cimis_csv_path)))
      out
    },
    
    stop("Invalid weather_source. Use 'open_meteo', 'daymet', or 'cimis_csv'.")
  )
  
  raw_avg_temp <- (weather_data$tmax + weather_data$tmin) / 2
  if (is.na(upper_thresh)) {
    weather_data$avg_temp <- raw_avg_temp
  } else {
    weather_data$avg_temp <- pmin(raw_avg_temp, upper_thresh)
  }
  weather_data$DD <- pmax(weather_data$avg_temp - lower_thresh, 0)
  
  n_na_dd <- sum(is.na(weather_data$DD))
  if (n_na_dd > 0) {
    msg <- sprintf("%d day(s) had missing temperature data and were treated as 0 degree-days.", n_na_dd)
    if (n_na_dd > 3 || n_na_dd / nrow(weather_data) > 0.02) warning(msg, call. = FALSE) else message(paste("Note:", msg))
    weather_data$DD[is.na(weather_data$DD)] <- 0
  }
  weather_data$cumulative_dd <- cumsum(weather_data$DD)
  
  final_df <- merge(trap_data, weather_data, by.x = "date", by.y = "Date", all.x = TRUE)
  if (nrow(final_df) != nrow(trap_data)) {
    warning(sprintf("Row count changed after merge (%d trap rows -> %d merged rows). Check for duplicate dates.",
                    nrow(trap_data), nrow(final_df)), call. = FALSE)
  }
  if (any(is.na(final_df$cumulative_dd))) {
    warning("Some trap dates have no matching weather data. Check weather_source coverage.", call. = FALSE)
  }
  
  biofix_date <- suppressWarnings(min(final_df$date[final_df$trap_counts > 0], na.rm = TRUE))
  if (is.infinite(biofix_date) || is.na(biofix_date)) stop("No positive trap counts found; cannot set biofix.")
  biofix_dd <- final_df$cumulative_dd[final_df$date == biofix_date][1]
  final_df$cumulative_dd_from_biofix <- final_df$cumulative_dd - biofix_dd
  
  attr(final_df, "lat")  <- lat
  attr(final_df, "lon")  <- lon
  attr(final_df, "year") <- data_year
  attr(final_df, "flight_interval_dd") <- flight_interval
  attr(final_df, "pest_label") <- pest_label
  attr(final_df, "upper_thresh_defined") <- !is.na(upper_thresh)
  
  final_df
}

#' Plot Pest Phenology as Trap Catch vs. Accumulated Degree-Days
#'
#' @param pheno_data A data frame produced by [calc_pest_phenology()],
#'   containing `cumulative_dd_from_biofix` and `trap_counts` columns.
#' @param pest Character. A pest code used to explore the pest name and
#'   flight interval from the built-in database, if not already present as
#'   attributes on `pheno_data`.
#' @param year Character or numeric. Year to display in the plot title.
#'   Defaults to the year attribute stored on `pheno_data`.
#' @param lat Numeric. Latitude to display in the plot title. Defaults to
#'   the `lat` attribute stored on `pheno_data`.
#' @param lon Numeric. Longitude to display in the plot title. Defaults to
#'   the `lon` attribute stored on `pheno_data`.
#' @param custom_flight_interval Numeric. Degree-day interval between
#'   generations/flights, used to draw flight lines. Overrides any interval
#'   stored on `pheno_data` or looked up via `pest`.
#' @param custom_pest_label Character. Custom pest label for the plot title
#'   when `pest` is not found in the built-in database.
#'
#' @return A `ggplot` object.
#' @export
plot_trap_phenology <- function(pheno_data, pest = NULL, year = NULL, lat = NULL, lon = NULL,
                                custom_flight_interval = NULL, custom_pest_label = NULL) {
  
  pheno_data$cumulative_dd_from_biofix <- as.numeric(pheno_data$cumulative_dd_from_biofix)
  
  if (is.null(year)) {
    year <- attr(pheno_data, "year")
    if (is.null(year) && "date" %in% names(pheno_data)) year <- format(min(pheno_data$date, na.rm = TRUE), "%Y")
  }
  if (is.null(lat)) lat <- attr(pheno_data, "lat")
  if (is.null(lon)) lon <- attr(pheno_data, "lon")
  
  pest_info <- NULL
  if (!is.null(pest)) {
    pest_info <- TrackTrap::pest_thresholds[TrackTrap::pest_thresholds$pest_code == toupper(pest), ]
  }
  
  interval <- NA
  if (!is.null(custom_flight_interval)) {
    interval <- custom_flight_interval
  } else if (!is.null(attr(pheno_data, "flight_interval_dd")) && !is.na(attr(pheno_data, "flight_interval_dd"))) {
    interval <- attr(pheno_data, "flight_interval_dd")
  } else if (!is.null(pest_info) && nrow(pest_info) == 1) {
    interval <- pest_info$flight_interval_dd
  }
  
  year_suffix <- if (!is.null(year)) sprintf(" (%s)", year) else ""
  location_suffix <- if (!is.null(lat) && !is.null(lon)) sprintf(" @ (%.4f, %.4f)", as.numeric(lat), as.numeric(lon)) else ""
  
  base_title <- if (!is.null(attr(pheno_data, "pest_label"))) {
    sprintf("%s Phenology", attr(pheno_data, "pest_label"))
  } else if (!is.null(pest_info) && nrow(pest_info) == 1) {
    sprintf("%s (%s) Phenology", pest_info$pest_name, pest_info$pest_code)
  } else if (!is.null(custom_pest_label)) {
    sprintf("%s Phenology", custom_pest_label)
  } else if (!is.null(pest)) {
    sprintf("%s Phenology", toupper(pest))
  } else "Pest Phenology"
  
  p <- ggplot2::ggplot(pheno_data, ggplot2::aes(x = cumulative_dd_from_biofix, y = trap_counts)) +
    ggplot2::geom_line(color = "darkred", linewidth = 1.2) +
    ggplot2::geom_point(color = "red", size = 3) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = paste0(base_title, year_suffix, location_suffix),
                  subtitle = "Trap Catch vs. Accumulated Degree-Days",
                  x = "Accumulated Degree-Days from Biofix", y = "Trap Catch Count") +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14),
                   axis.title = ggplot2::element_text(face = "bold"))
  
  if (!is.null(interval) && !is.na(interval) && interval > 0) {
    max_dd <- suppressWarnings(max(pheno_data$cumulative_dd_from_biofix, na.rm = TRUE))
    n_flights <- max(1, ceiling(max_dd / interval) + 1)
    flights <- interval * (0:(n_flights - 1))
    ordinals <- c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th")
    labels <- vapply(seq_along(flights), function(i) {
      ord <- if (i <= length(ordinals)) ordinals[i] else paste0(i, "th")
      if (i == 1) paste0(ord, " Flight\n(Biofix)") else paste0(ord, " Flight")
    }, character(1))
    y_pos <- max(pheno_data$trap_counts, na.rm = TRUE) * seq(0.95, 0.65, length.out = length(flights))
    
    for (i in seq_along(flights)) {
      p <- p +
        ggplot2::geom_vline(xintercept = flights[i], linetype = "dashed", color = "blue", linewidth = 0.8) +
        ggplot2::annotate("text", x = flights[i] + interval * 0.03, y = y_pos[i],
                          label = labels[i], hjust = 0, color = "blue", size = 3.5)
    }
  } else if (!is.null(pest) || !is.null(custom_pest_label)) {
    message("Note: No generation interval defined or provided for this pest. Flight lines skipped.")
  }
  
  p
}