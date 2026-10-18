test_that("biofix exclusion and cumulative DD match hand calculations", {
  dates <- seq(as.Date("2024-04-01"), as.Date("2024-04-22"), by = "day")
  path <- make_cimis_csv(dates)
  on.exit(unlink(path), add = TRUE)
  trap <- data.frame(date = dates[c(1, 8, 15, 22)], trap_counts = c(0, 0, 3, 9))
  x <- calc_pest_phenology(trap, custom_lower = 60, upper_cutoff = "none",
                           weather_source = "cimis_csv", cimis_csv_path = path)
  expect_equal(x$DD, rep(10, 4))
  expect_equal(x$cumulative_dd, c(10, 80, 150, 220))
  expect_equal(x$cumulative_dd_from_biofix, c(NA_real_, NA_real_, 0, 70))
  expect_equal(attr(x, "daily_weather")$Date, dates)
  expect_equal(attr(x, "daily_weather")$DD, rep(10, 22))
  meta <- attr(x, "calculation_metadata")
  expect_identical(meta$biofix_rule, "first_capture")
  expect_false(meta$include_biofix_day)
  expect_equal(meta$weather_start, "2024-04-01")
  expect_equal(meta$accumulation_start, "2024-04-16")
})

test_that("daily-mean upper capping and zero flooring are correct", {
  dates <- as.Date("2024-04-01") + 0:2
  trap <- data.frame(date = dates, trap_counts = c(1, 0, 2))
  # Day 2 distinguishes capping the mean from capping Tmax separately.
  weather <- data.frame(Date = dates, tmin = c(40, 50, 90), tmax = c(50, 110, 110))
  x <- calc_pest_phenology(trap, custom_lower = 60, custom_upper = 90,
                           upper_cutoff = "horizontal", weather_data = weather,
                           weather_provenance = "Synthetic test data")
  expect_equal(x$DD, c(0, 20, 30))
  expect_equal(x$cumulative_dd_from_biofix, c(0, 20, 50))
  expect_true(attr(x, "upper_thresh_defined"))
})

test_that("missing upper requires explicit selection and NA alone is insufficient", {
  trap <- data.frame(date = as.Date("2024-04-01") + 0:1, trap_counts = c(1, 2))
  weather <- data.frame(Date = trap$date, tmin = 100, tmax = 120)
  expect_error(calc_pest_phenology(trap, custom_lower = 50, custom_upper = NA_real_),
               "Upper threshold is unknown", fixed = TRUE)
  x <- calc_pest_phenology(trap, custom_lower = 50, custom_upper = NA_real_,
                           upper_cutoff = "none", weather_data = weather, weather_provenance = "Synthetic")
  expect_equal(x$DD, c(60, 60))
  expect_equal(x$cumulative_dd_from_biofix, c(0, 60))
  expect_false(attr(x, "upper_thresh_defined"))
  expect_error(calc_pest_phenology(trap, custom_lower = 50, custom_upper = 90,
                                   upper_cutoff = "none"), "conflicts", fixed = TRUE)
  expect_error(calc_pest_phenology(trap, custom_lower = 50, custom_upper = 50,
                                   upper_cutoff = "horizontal"), "greater than the lower", fixed = TRUE)
})

test_that("database overrides retain remaining parameters and provenance", {
  db <- data.frame(pest_code = "TEST", pest_name = "Synthetic pest",
                   lower_thresh = 50, upper_thresh = 90, thermal_requirement_dd = 100,
                   thermal_requirement_endpoint = "Synthetic endpoint", lower_source = "Synthetic source")
  trap <- data.frame(date = as.Date("2024-04-01") + 0:1, trap_counts = c(1, 2))
  weather <- data.frame(Date = trap$date, tmin = 55, tmax = 85)
  x <- calc_pest_phenology(trap, pest = "TEST", database = db, custom_lower = 65,
                           weather_data = weather, weather_provenance = "Synthetic")
  meta <- attr(x, "calculation_metadata")
  expect_equal(x$DD, c(5, 5))
  expect_equal(meta$upper_thresh, 90)
  expect_equal(meta$lower_thresh, 65)
  expect_equal(meta$database_record$lower_source, "Synthetic source")
  expect_true(is.na(attr(x, "thermal_requirement_dd")))
  db$lower_thresh <- NA_real_
  expect_error(calc_pest_phenology(trap, pest = "TEST", database = db),
               "No lower threshold available", fixed = TRUE)
  expect_error(calc_pest_phenology(trap, pest = "UNKNOWN", database = db,
                                   custom_lower = 50, upper_cutoff = "none"), "Unknown pest code", fixed = TRUE)
  expect_error(calc_pest_phenology(trap), "No lower threshold available", fixed = TRUE)
})

test_that("supported trap and CIMIS date formats agree", {
  dates <- as.Date("2024-04-01") + 0:1
  for (fmt in c("%m/%d/%Y", "%Y-%m-%d")) {
    path <- make_cimis_csv(dates, date_fmt = fmt)
    for (input in list(dates, c("4/1/2024", "4/2/2024"),
                       c("04/01/2024", "04/02/2024"), as.character(dates))) {
      x <- calc_pest_phenology(data.frame(date = input, trap_counts = c(1, 2)),
                               custom_lower = 50, upper_cutoff = "none", weather_source = "cimis_csv",
                               cimis_csv_path = path)
      expect_equal(x$date, dates)
      expect_equal(x$cumulative_dd_from_biofix, c(0, 20))
    }
    unlink(path)
  }
})

test_that("invalid trap inputs fail before retrieval", {
  trap <- data.frame(date = c("2024-04-01", "2024-04-02"), trap_counts = c(1, 2))
  for (bad in c("2024-04-02junk", "02-Apr-2024", "2024-02-30")) {
    broken <- trap; broken$date[2] <- bad
    expect_error(calc_pest_phenology(broken, custom_lower = 50, upper_cutoff = "none"),
                 "[Dd]ates")
  }
  expect_error(calc_pest_phenology(trap["date"]), "trap_counts", fixed = TRUE)
  for (counts in list(c(-1, 2), c(1.5, 2), c(NA, 2), c("zero", "three"))) {
    broken <- trap; broken$trap_counts <- counts
    expect_error(calc_pest_phenology(broken), "[Tt]rap.*count")
  }
  broken <- trap; broken$date[2] <- broken$date[1]
  expect_error(calc_pest_phenology(broken), "one row per date", fixed = TRUE)
  broken <- trap; broken$date[2] <- "2025-04-02"
  expect_error(calc_pest_phenology(broken), "monitoring year", fixed = TRUE)
  broken <- trap; broken$trap_counts <- c(0, 0)
  expect_error(calc_pest_phenology(broken, custom_lower = 50, upper_cutoff = "none"),
               "No positive trap catches", fixed = TRUE)
})

test_that("weather configuration errors are specific", {
  trap <- data.frame(date = as.Date("2024-04-01") + 0:1, trap_counts = c(1, 2))
  expect_error(calc_pest_phenology(trap, custom_lower = 50, upper_cutoff = "none",
                                   weather_source = "cimis_csv"), "Supply cimis_csv_path", fixed = TRUE)
  for (provider in c("open_meteo", "daymet")) {
    expect_error(calc_pest_phenology(trap, custom_lower = 50, upper_cutoff = "none",
                                     weather_source = provider), "lat and lon", fixed = TRUE)
  }
  expect_error(calc_pest_phenology(trap, custom_lower = 50, upper_cutoff = "none",
                                   weather_source = "invalid"), "Use open_meteo", fixed = TRUE)
  weather <- data.frame(Date = trap$date, tmin = 55, tmax = 85)
  expect_error(calc_pest_phenology(trap, custom_lower = 50, upper_cutoff = "none",
                                   weather_data = weather), "weather_provenance", fixed = TRUE)
})

test_that("incomplete or invalid daily weather cannot silently accumulate", {
  dates <- as.Date("2024-04-01") + 0:2
  trap <- data.frame(date = dates[c(1, 3)], trap_counts = c(1, 2))
  good <- data.frame(Date = dates, tmin = 55, tmax = 85)
  cases <- list(good[-2, ], rbind(good, good[1, ]), good, good)
  cases[[3]]$tmax[2] <- NA_real_
  cases[[4]]$tmin[2] <- 95
  messages <- c("Missing daily weather", "Duplicate daily weather",
                "Daily tmax contains", "minima exceed maxima")
  for (i in seq_along(cases)) {
    expect_error(calc_pest_phenology(trap, custom_lower = 50, upper_cutoff = "none",
                                     weather_data = cases[[i]], weather_provenance = "Synthetic"),
                 messages[i], fixed = TRUE)
  }
})
