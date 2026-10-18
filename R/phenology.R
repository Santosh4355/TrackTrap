#' Fetch daily historical Open-Meteo temperatures in Fahrenheit
#' @param lat,lon Coordinates.
#' @param start_date,end_date Requested dates, inclusive.
#' @param timezone IANA timezone or "auto"
#' @param model Explicit archive model identifier.
#' @export
fetch_open_meteo <- function(lat, lon, start_date, end_date = as.character(Sys.Date()),
                             timezone = "auto", model = "era5_land") {
  if (!is.numeric(lat) || length(lat) != 1L || !is.finite(lat) ||
      !is.numeric(lon) || length(lon) != 1L || !is.finite(lon))
    stop("lat and lon must each be one finite number.", call. = FALSE)
  if (abs(lat) > 90 || abs(lon) > 180) stop("Invalid coordinates.", call. = FALSE)
  query <- list(latitude = lat, longitude = lon, start_date = as.character(start_date),
                end_date = as.character(end_date), daily = "temperature_2m_max,temperature_2m_min",
                temperature_unit = "fahrenheit", timezone = timezone, models = model)
  response <- httr::GET("https://archive-api.open-meteo.com/v1/archive", query = query)
  httr::stop_for_status(response)
  parsed <- httr::content(response, as = "parsed", type = "application/json")
  daily <- parsed$daily
  if (is.null(daily$time) || is.null(daily$temperature_2m_max) ||
      is.null(daily$temperature_2m_min)) stop("Incomplete Open-Meteo response.", call. = FALSE)
  for (field in c("temperature_2m_max", "temperature_2m_min")) {
    values <- daily[[field]]
    values[vapply(values, is.null, logical(1))] <- NA_real_
    daily[[field]] <- as.numeric(unlist(values))
  }
  out <- data.frame(Date = as.Date(unlist(daily$time)),
                    tmax = daily$temperature_2m_max,
                    tmin = daily$temperature_2m_min)
  attr(out, "provenance") <- list(provider = "Open-Meteo historical archive", request = query,
                                  resolved_timezone = parsed$timezone, grid_latitude = parsed$latitude,
                                  grid_longitude = parsed$longitude, elevation = parsed$elevation,
                                  units = parsed$daily_units, retrieved_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE))
  out
}

#' Calculate a simple-average degree-day index for trap observations
#'
#' @param trap_data Data frame with date and trap_counts, one row per date.
#' @param pest Optional database code.
#' @param lat,lon Weather coordinates.
#' @param weather_source open_meteo, daymet, cimis_csv, or supplied.
#' @param cimis_csv_path Path to CIMIS Fahrenheit daily report.
#' @param custom_lower,custom_upper Optional Fahrenheit threshold overrides.
#' @param custom_thermal_requirement Optional user-defined thermal marker (Fahrenheit DD).
#' @param custom_flight_interval Deprecated alias for custom_thermal_requirement.
#' @param custom_pest_label Optional plot label.
#' @param upper_cutoff "explicit", "none", or "horizontal". Missing database
#'   upper thresholds require an explicit "none" choice or a supplied upper.
#' @details Biofix is always the first positive trap count; its DD is excluded.
#'   Daily DD is max(min(mean temperature, upper) - lower, 0), or no upper cap.
#' @param weather_data Optional daily Date, tmin, tmax data in Fahrenheit.
#' @param weather_provenance Description of provided temperatures/station/time convention.
#' @param timezone,open_meteo_model Open-Meteo settings.
#' @param database Optional database; defaults to TrackTrap::pest_thresholds.
#' @return Original data frame. 
#' @export
calc_pest_phenology <- function(trap_data, pest = NULL, lat = NULL, lon = NULL,
                                weather_source = "open_meteo", cimis_csv_path = NULL,
                                custom_lower = NULL, custom_upper = NULL,
                                custom_thermal_requirement = NULL,
                                custom_flight_interval = NULL, custom_pest_label = NULL,
                                upper_cutoff = c("explicit", "none", "horizontal"),
                                weather_data = NULL,
                                weather_provenance = NULL, timezone = "auto",
                                open_meteo_model = "era5_land", database = NULL) {
  upper_cutoff <- match.arg(upper_cutoff)
  biofix_rule <- "first_capture"
  include_biofix_day <- FALSE
  if (!is.null(custom_flight_interval)) {
    warning("custom_flight_interval is deprecated; use custom_thermal_requirement.", call. = FALSE)
    if (!is.null(custom_thermal_requirement)) stop("Provide only one thermal-requirement argument.")
    custom_thermal_requirement <- custom_flight_interval
  }
  custom_flight_interval <- custom_thermal_requirement
  if (!all(c("date", "trap_counts") %in% names(trap_data)))
    stop("trap_data requires date and trap_counts.", call. = FALSE)
  if (!inherits(trap_data$date, "Date")) {
    date_text <- trimws(as.character(trap_data$date))
    if (anyNA(date_text)) stop("trap_data$date contains missing dates.", call. = FALSE)
    iso <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", date_text)
    us <- grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", date_text)
    if (any(!(iso | us))) stop("Dates must use YYYY-MM-DD or M/D/YYYY.", call. = FALSE)
    parsed_dates <- as.Date(rep(NA_character_, length(date_text)))
    parsed_dates[iso] <- as.Date(date_text[iso], format = "%Y-%m-%d")
    parsed_dates[!iso] <- as.Date(date_text[!iso], format = "%m/%d/%Y")
    trap_data$date <- parsed_dates
  }
  if (!length(trap_data$date) || anyNA(trap_data$date))
    stop("trap_data$date requires valid, nonmissing dates (YYYY-MM-DD or M/D/YYYY).", call. = FALSE)
  trap_data$trap_counts <- suppressWarnings(as.numeric(as.character(trap_data$trap_counts)))
  if (any(!is.finite(trap_data$trap_counts))) stop("trap_counts contains missing or invalid values.", call. = FALSE)
  if (any(trap_data$trap_counts < 0) || any(trap_data$trap_counts != floor(trap_data$trap_counts)))
    stop("Trap counts must be nonnegative integers.", call. = FALSE)
  if (anyDuplicated(trap_data$date)) stop("Use one trap series with one row per date.", call. = FALSE)
  trap_data <- trap_data[order(trap_data$date), , drop = FALSE]
  if (length(unique(format(trap_data$date, "%Y"))) != 1L)
    stop("Analyze each monitoring year separately.", call. = FALSE)
  
  pest_info <- NULL
  if (!is.null(pest)) {
    if (!is.character(pest) || length(pest) != 1L || is.na(pest) || !nzchar(trimws(pest))) stop("pest must be one code.", call. = FALSE)
    pest <- toupper(trimws(pest))
    if (is.null(database)) database <- TrackTrap::pest_thresholds
    required <- c("pest_code", "pest_name", "lower_thresh", "upper_thresh")
    if (!all(required %in% names(database))) stop("Database lacks required pest/threshold columns.")
    pest_info <- database[!is.na(database$pest_code) & database$pest_code == pest, , drop = FALSE]
    if (nrow(pest_info) > 1L) stop("Duplicate pest code in database.", call. = FALSE)
    if (nrow(pest_info) == 0L) stop("Unknown pest code. Use pest = NULL for a custom analysis.", call. = FALSE)
  }
  found <- !is.null(pest_info) && nrow(pest_info) == 1L
  database_requirement <- NA_real_
  database_endpoint <- NA_character_
  if (found) {
    field <- if ("thermal_requirement_dd" %in% names(pest_info)) "thermal_requirement_dd" else "flight_interval_dd"
    if (field %in% names(pest_info)) database_requirement <- pest_info[[field]]
    if ("thermal_requirement_endpoint" %in% names(pest_info)) database_endpoint <- pest_info$thermal_requirement_endpoint
  }
  lower_thresh <- if (!is.null(custom_lower)) custom_lower else if (found) pest_info$lower_thresh else NA_real_
  upper_thresh <- if (!is.null(custom_upper)) custom_upper else if (found) pest_info$upper_thresh else NA_real_
  if (length(lower_thresh) != 1L || is.na(lower_thresh))
    stop("No lower threshold available. Supply custom_lower to calculate degree-days.", call. = FALSE)
  if (!is.numeric(lower_thresh) || length(lower_thresh) != 1L || !is.finite(lower_thresh))
    stop("lower_thresh must be one finite number.", call. = FALSE)
  if (is.logical(upper_thresh) && length(upper_thresh) == 1L && is.na(upper_thresh))
    upper_thresh <- NA_real_
  if (!is.numeric(upper_thresh) || length(upper_thresh) != 1L ||
      is.nan(upper_thresh) || (!is.na(upper_thresh) && !is.finite(upper_thresh)))
    stop("upper_thresh must be one finite number or NA.", call. = FALSE)
  if (upper_cutoff == "explicit") {
    if (is.na(upper_thresh)) stop("Upper threshold is unknown. Choose upper_cutoff='none' explicitly or supply custom_upper.", call. = FALSE)
    upper_cutoff <- "horizontal"
  }
  if (upper_cutoff == "horizontal" && (is.na(upper_thresh) || upper_thresh <= lower_thresh))
    stop("Horizontal cutoff requires a finite upper threshold greater than the lower.", call. = FALSE)
  if (upper_cutoff == "none" && !is.null(custom_upper) && !is.na(custom_upper))
    stop("A finite custom_upper conflicts with upper_cutoff='none'.", call. = FALSE)
  supplied_upper <- upper_thresh
  if (upper_cutoff == "none") upper_thresh <- NA_real_
  if (!is.null(custom_flight_interval)) {
    if (!is.numeric(custom_flight_interval) || length(custom_flight_interval) != 1L || !is.finite(custom_flight_interval))
      stop("custom_thermal_requirement must be one finite number.", call. = FALSE)
    if (custom_flight_interval <= 0) stop("custom_thermal_requirement must be positive.")
  }
  pest_label <- if (!is.null(custom_pest_label)) custom_pest_label else if (found)
    sprintf("%s (%s)", pest_info$pest_name, pest) else if (!is.null(pest)) pest else "Custom Pest"
  
  positive <- trap_data$date[trap_data$trap_counts > 0]
  if (!length(positive)) stop("No positive trap catches; first-capture biofix is unavailable.")
  biofix_date <- min(positive)
  fetch_start <- min(min(trap_data$date), biofix_date)
  fetch_end <- max(trap_data$date)
  if (biofix_date > fetch_end) stop("Biofix cannot be later than the final observation.")
  if (is.null(weather_data)) {
    weather_data <- switch(weather_source,
                           open_meteo = fetch_open_meteo(lat, lon, fetch_start, fetch_end, timezone, open_meteo_model),
                           daymet = {
                             if (!is.numeric(lat) || length(lat) != 1L || !is.finite(lat) ||
                                 !is.numeric(lon) || length(lon) != 1L || !is.finite(lon))
                               stop("lat and lon must each be one finite number.", call. = FALSE)
                             if (abs(lat) > 90 || abs(lon) > 180) stop("Invalid coordinates.")
                             raw <- daymetr::download_daymet(lat = lat, lon = lon,
                                                             start = as.integer(format(fetch_start, "%Y")), end = as.integer(format(fetch_end, "%Y")),
                                                             internal = TRUE, silent = TRUE)
                             dat <- raw$data
                             required <- c("year", "yday", "tmax..deg.c.", "tmin..deg.c.")
                             if (!all(required %in% names(dat))) stop("Unexpected daymetr output columns.")
                             out <- data.frame(Date = as.Date(paste(dat$year, dat$yday, sep = "-"), "%Y-%j"),
                                               tmax = dat$tmax..deg.c. * 1.8 + 32, tmin = dat$tmin..deg.c. * 1.8 + 32)
                             attr(out, "provenance") <- list(provider = "Daymet via daymetr", latitude = lat, longitude = lon,
                                                             daymetr_version = as.character(utils::packageVersion("daymetr")),
                                                             source_metadata = raw[setdiff(names(raw), "data")],
                                                             calendar = "Native year/yday; leap years omit December 31. No day shifting or filling.",
                                                             retrieved_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE))
                             out
                           },
                           cimis_csv = {
                             if (is.null(cimis_csv_path)) stop("Supply cimis_csv_path.")
                             raw <- utils::read.csv(cimis_csv_path, stringsAsFactors = FALSE)
                             required <- c("Date", "Max.Air.Temp..F.", "Min.Air.Temp..F.")
                             if (!all(required %in% names(raw))) stop("CIMIS CSV requires Date, Max.Air.Temp..F., Min.Air.Temp..F.")
                             out <- data.frame(Date = raw$Date, tmax = raw$Max.Air.Temp..F., tmin = raw$Min.Air.Temp..F.)
                             attr(out, "provenance") <- list(provider = "User-supplied CIMIS report", filename = basename(cimis_csv_path),
                                                             file_md5 = unname(tools::md5sum(cimis_csv_path)), station_and_time_convention = weather_provenance)
                             out
                           }, stop("Use open_meteo, daymet, cimis_csv or provide weather_data."))
  } else {
    weather_source <- "supplied"
    if (is.null(weather_provenance) || !length(weather_provenance))
      stop("Provide weather_provenance for supplied temperatures.")
    attr(weather_data, "provenance") <- weather_provenance
  }
  provenance <- attr(weather_data, "provenance")
  if (!all(c("Date", "tmax", "tmin") %in% names(weather_data))) stop("Weather requires Date, tmax and tmin in Fahrenheit.")
  if (!inherits(weather_data$Date, "Date")) {
    date_text <- trimws(as.character(weather_data$Date))
    if (anyNA(date_text)) stop("weather Date contains missing dates.", call. = FALSE)
    iso <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", date_text)
    us <- grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", date_text)
    if (any(!(iso | us))) stop("Dates must use YYYY-MM-DD or M/D/YYYY.", call. = FALSE)
    parsed_dates <- as.Date(rep(NA_character_, length(date_text)))
    parsed_dates[iso] <- as.Date(date_text[iso], format = "%Y-%m-%d")
    parsed_dates[!iso] <- as.Date(date_text[!iso], format = "%m/%d/%Y")
    weather_data$Date <- parsed_dates
  }
  if (!length(weather_data$Date) || anyNA(weather_data$Date))
    stop("weather Date requires valid, nonmissing dates (YYYY-MM-DD or M/D/YYYY).", call. = FALSE)
  weather_data <- weather_data[weather_data$Date >= fetch_start & weather_data$Date <= fetch_end, , drop = FALSE]
  if (anyDuplicated(weather_data$Date)) stop("Duplicate daily weather dates; select one station/series.")
  expected <- seq(fetch_start, fetch_end, by = "day")
  absent <- expected[!expected %in% weather_data$Date]
  if (length(absent)) stop("Missing daily weather: ", paste(as.character(utils::head(absent, 10)), collapse = ", "))
  weather_data <- weather_data[match(expected, weather_data$Date), , drop = FALSE]
  weather_data$tmax <- suppressWarnings(as.numeric(as.character(weather_data$tmax)))
  if (any(!is.finite(weather_data$tmax))) stop("Daily tmax contains missing or invalid values.", call. = FALSE)
  weather_data$tmin <- suppressWarnings(as.numeric(as.character(weather_data$tmin)))
  if (any(!is.finite(weather_data$tmin))) stop("Daily tmin contains missing or invalid values.", call. = FALSE)
  if (any(weather_data$tmin > weather_data$tmax)) stop("Some daily minima exceed maxima.")
  weather_data$raw_avg_temp <- (weather_data$tmax + weather_data$tmin) / 2
  weather_data$avg_temp <- if (upper_cutoff == "none") weather_data$raw_avg_temp else
    pmin(weather_data$raw_avg_temp, upper_thresh)
  weather_data$DD <- pmax(weather_data$avg_temp - lower_thresh, 0)
  weather_data$cumulative_dd <- cumsum(weather_data$DD)
  eligible <- if (include_biofix_day) weather_data$Date >= biofix_date else weather_data$Date > biofix_date
  weather_data$cumulative_dd_from_biofix <- cumsum(ifelse(eligible, weather_data$DD, 0))
  weather_data$cumulative_dd_from_biofix[weather_data$Date < biofix_date] <- NA_real_
  reserved <- intersect(setdiff(names(weather_data), "Date"), names(trap_data))
  if (length(reserved)) stop("Trap data contains reserved weather/output columns: ", paste(reserved, collapse = ", "))
  final_df <- merge(trap_data, weather_data, by.x = "date", by.y = "Date", all.x = TRUE, sort = TRUE)
  status <- "Configured simple-average DD analysis; biological model compatibility not established"
  metadata <- list(pest = pest, pest_label = pest_label, analysis_status = status,
                   lower_thresh = lower_thresh, upper_thresh = upper_thresh, database_or_supplied_upper = supplied_upper,
                   lower_origin = if (!is.null(custom_lower)) "user" else "database",
                   upper_origin = if (upper_cutoff == "none") "user-selected no cutoff" else if (!is.null(custom_upper)) "user" else "database",
                   units = "Fahrenheit temperatures; Fahrenheit degree-days", method = "simple_average",
                   upper_cutoff = upper_cutoff, biofix_date = as.character(biofix_date), biofix_rule = biofix_rule,
                   include_biofix_day = include_biofix_day, pre_biofix = "NA (not accumulated)",
                   weather_start = as.character(fetch_start),
                   accumulation_start = as.character(biofix_date + 1), end_date = as.character(fetch_end),
                   weather_source = weather_source, weather_provenance = provenance,
                   database_record = if (found) as.list(pest_info[1, , drop = FALSE]) else NULL,
                   database_thermal_requirement_dd = database_requirement,
                   database_thermal_requirement_endpoint = database_endpoint,
                   database_sources = if (found) as.list(pest_info[intersect(names(pest_info),
                                                                             c("lower_source", "upper_source", "thermal_requirement_source"))]) else NULL,
                   custom_thermal_requirement = custom_thermal_requirement,
                   custom_flight_interval = custom_flight_interval, missing_weather_policy = "stop; no zero substitution")
  attr(final_df, "lat") <- lat; attr(final_df, "lon") <- lon
  attr(final_df, "year") <- format(max(trap_data$date), "%Y")
  attr(final_df, "pest_label") <- pest_label
  attr(final_df, "thermal_requirement_dd") <- if (is.null(custom_thermal_requirement)) NA_real_ else custom_thermal_requirement
  attr(final_df, "thermal_requirement_endpoint") <- if (is.null(custom_thermal_requirement)) NA_character_ else "User-defined reference from first capture; biological compatibility unvalidated"
  attr(final_df, "upper_thresh_defined") <- !is.na(upper_thresh)
  attr(final_df, "daily_weather") <- weather_data
  attr(final_df, "calculation_metadata") <- metadata
  message(status)
  final_df
}

#' Plot trap catches against the calculated DD index
#' @param pheno_data Output of calc_pest_phenology().
#' @param pest Optional label only; never used to retrieve marker intervals.
#' @param year,lat,lon Optional title settings.
#' @param custom_thermal_requirement Optional user-defined marker in Fahrenheit DD.
#' @param repeat_markers If TRUE, show user-defined multiples; default FALSE.
#' @param custom_flight_interval Deprecated alias for custom_thermal_requirement.
#' @param custom_pest_label Optional title label.
#' @details When a thermal marker is selected, the subtitle displays its value.
#'   A matching database reference supplies the developmental endpoint; otherwise
#'   the endpoint is reported as unspecified. The caption explains the reference
#'   and distinguishes first-capture multiples from observed generations.
#'   Database requirements are not automatically selected as markers.
#' @export
plot_trap_phenology <- function(pheno_data, pest = NULL, year = NULL, lat = NULL, lon = NULL,
                                custom_thermal_requirement = NULL, repeat_markers = FALSE,
                                custom_flight_interval = NULL, custom_pest_label = NULL) {
  if (!all(c("cumulative_dd_from_biofix", "trap_counts") %in% names(pheno_data))) stop("Missing phenology columns.")
  metadata <- attr(pheno_data, "calculation_metadata")
  if (!is.logical(repeat_markers) || length(repeat_markers) != 1L || is.na(repeat_markers)) stop("repeat_markers must be TRUE or FALSE.")
  if (!is.null(custom_flight_interval)) {
    warning("custom_flight_interval is deprecated; use custom_thermal_requirement.", call. = FALSE)
    if (!is.null(custom_thermal_requirement)) stop("Supply only one thermal-requirement argument.")
    custom_thermal_requirement <- custom_flight_interval
  }
  interval <- custom_thermal_requirement
  if (is.null(interval) && !is.null(metadata)) interval <- metadata$custom_thermal_requirement
  if (!is.null(interval)) {
    if (!is.numeric(interval) || length(interval) != 1L || !is.finite(interval))
      stop("custom_thermal_requirement must be one finite number.", call. = FALSE)
    if (interval <= 0) stop("Marker spacing must be positive.")
  }
  # Explain selected thermal markers automatically, using retained provenance.
  thermal_subtitle <- NULL
  thermal_caption <- NULL
  if (!is.null(interval)) {
    endpoint <- "developmental endpoint not specified"
    reference <- metadata$database_thermal_requirement_dd
    reference_endpoint <- metadata$database_thermal_requirement_endpoint
    if (is.numeric(reference) && length(reference) == 1L &&
        is.finite(reference) &&
        isTRUE(all.equal(interval, reference, tolerance = 1e-8)) &&
        is.character(reference_endpoint) && length(reference_endpoint) == 1L &&
        !is.na(reference_endpoint) && nzchar(trimws(reference_endpoint))) {
      endpoint <- paste0(trimws(reference_endpoint), " (database reference)")
    }
    thermal_subtitle <- paste(strwrap(sprintf(
      "Thermal requirement: %.1f \u00B0F-days; %s", interval, endpoint
    ), width = 95), collapse = "\n")
    thermal_caption <- paste(
      "Thermal requirement = accumulated heat associated with a specified developmental endpoint.",
      if (repeat_markers)
        "Dotted lines mark multiples of this reference value measured from first capture." else
          "The dotted line marks this reference value measured from first capture.",
      sep = "\n"
    )
  }
  label <- custom_pest_label
  if (is.null(label)) label <- attr(pheno_data, "pest_label")
  if (is.null(label)) label <- if (is.null(pest)) "Trap observations" else pest
  if (is.null(year)) year <- attr(pheno_data, "year")
  if (is.null(lat)) lat <- attr(pheno_data, "lat")
  if (is.null(lon)) lon <- attr(pheno_data, "lon")
  title <- paste0(label, if (!is.null(year)) paste0(" (", year, ")") else "",
                  if (!is.null(lat) && !is.null(lon)) sprintf(" @ (%.4f, %.4f)", lat, lon) else "")
  keep <- is.finite(pheno_data$cumulative_dd_from_biofix) & is.finite(pheno_data$trap_counts)
  dat <- pheno_data[keep, , drop = FALSE]
  if (!nrow(dat)) stop("No finite post-biofix observations to plot.")
  p <- ggplot2::ggplot(dat, ggplot2::aes(x = cumulative_dd_from_biofix, y = trap_counts)) +
    ggplot2::geom_line(color = "darkred", linewidth = 1.2) +
    ggplot2::geom_point(color = "red", size = 3) + ggplot2::theme_minimal() +
    ggplot2::labs(title = title, subtitle = thermal_subtitle,
                  caption = thermal_caption,
                  x = "Accumulated degree-days from first capture (\u00B0F-days)",
                  y = "Trap catch count") +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0),
                   plot.subtitle = ggplot2::element_text(size = 10, hjust = 0),
                   plot.caption = ggplot2::element_text(size = 8, hjust = 0),
                   axis.title = ggplot2::element_text(face = "bold"))
  p <- p + ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
    ggplot2::annotate("text", x = 0, y = max(dat$trap_counts),
                      label = "Biofix: first positive catch", hjust = 0, vjust = -0.4, size = 3)
  if (!is.null(interval)) {
    n <- if (repeat_markers) max(1, floor(max(dat$cumulative_dd_from_biofix) / interval)) else 1
    if (n > 100L) stop("More than 100 markers requested; increase the requirement.")
    markers <- seq_len(n) * interval
    p <- p + ggplot2::geom_vline(xintercept = markers, linetype = "dotted", color = "blue") +
      ggplot2::annotate("text", x = markers, y = max(dat$trap_counts),
                        label = paste0(seq_len(n), " \u00D7 thermal requirement"),
                        angle = 90, vjust = -0.3, hjust = 1, size = 3)
  }
  p
}
