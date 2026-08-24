make_cimis_csv <- function(dates, tmax, tmin, date_fmt = "%m/%d/%Y") {
  df <- data.frame(
    Date = format(dates, date_fmt),
    `Max Air Temp (F)` = tmax,
    `Min Air Temp (F)` = tmin,
    check.names = FALSE
  )
  path <- tempfile(fileext = ".csv")
  write.csv(df, path, row.names = FALSE)
  path
}

test_that("biofix is correctly set to zero and cumulative degree days agrees with manually calculated values", {
  dates <- seq(as.Date("2024-04-01"), as.Date("2024-05-15"), by = "day")
  csv_path <- make_cimis_csv(dates, tmax = 85, tmin = 55)
  
  trap_df <- data.frame(
    date = c("4/1/2024", "4/8/2024", "4/15/2024", "4/22/2024"),
    trap_counts = c(0, 0, 3, 9)
  )
  
  result <- calc_pest_phenology(
    trap_df, pest = "OLFF",
    weather_source = "cimis_csv",
    cimis_csv_path = csv_path
  )
  
  expect_s3_class(result, "data.frame")
  expect_true("cumulative_dd_from_biofix" %in% names(result))
  
  biofix_row <- result[result$date == as.Date("2024-04-15"), ]
  expect_equal(biofix_row$cumulative_dd_from_biofix, 0)
  
  row_01 <- result[result$date == as.Date("2024-04-22"), ]
  expect_equal(row_01$cumulative_dd, 220)
  expect_equal(row_01$cumulative_dd_from_biofix, 70)
  
  row_02 <- result[result$date == as.Date("2024-04-01"), ]
  expect_equal(row_02$cumulative_dd, 10)
  expect_equal(row_02$cumulative_dd_from_biofix, -140)
})

test_that("upper threshold caps daily dd as expected", {
  dates <- seq(as.Date("2024-04-01"), as.Date("2024-04-10"), by = "day")
  csv_path <- make_cimis_csv(dates, tmax = 110, tmin = 90)
  
  trap_df <- data.frame(
    date = c("4/1/2024", "4/10/2024"),
    trap_counts = c(1, 0)
  )
  
  result <- calc_pest_phenology(
    trap_df, pest = "OLFF",
    weather_source = "cimis_csv",
    cimis_csv_path = csv_path
  )
  
  expect_equal(result$cumulative_dd[result$date == as.Date("2024-04-01")], 35)
})

test_that("lower threshold floors daily dd at zero when avg temp is below threshold", {
  dates <- seq(as.Date("2024-04-01"), as.Date("2024-04-10"), by = "day")
  csv_path <- make_cimis_csv(dates, tmax = 55, tmin = 45)
  
  trap_df <- data.frame(date = c("4/1/2024", "4/10/2024"), trap_counts = c(1, 0))
  
  result <- calc_pest_phenology(
    trap_df, pest = "OLFF",
    weather_source = "cimis_csv",
    cimis_csv_path = csv_path
  )
  
  expect_equal(result$cumulative_dd[result$date == as.Date("2024-04-01")], 0)
})

test_that("both M/D/YYYY and MM/DD/YYYY parse identically in trap_data", {
  dates <- seq(as.Date("2024-04-01"), as.Date("2024-04-30"), by = "day")
  csv_path <- make_cimis_csv(dates, tmax = 85, tmin = 55)
  
  trap_df_unpadded <- data.frame(
    date = c("4/1/2024", "4/15/2024"),
    trap_counts = c(0, 3)
  )
  trap_df_padded <- data.frame(
    date = c("04/01/2024", "04/15/2024"),
    trap_counts = c(0, 3)
  )
  
  res_unpadded <- calc_pest_phenology(trap_df_unpadded, pest = "OLFF",
                                      weather_source = "cimis_csv", cimis_csv_path = csv_path)
  res_padded <- calc_pest_phenology(trap_df_padded, pest = "OLFF",
                                    weather_source = "cimis_csv", cimis_csv_path = csv_path)
  
  expect_equal(res_unpadded$cumulative_dd_from_biofix, res_padded$cumulative_dd_from_biofix)
})

test_that("invalid trap date format errors with the offending value named", {
  trap_df <- data.frame(
    date = c("4/1/2024", "2024-04-08"),
    trap_counts = c(0, 3)
  )
  expect_error(
    calc_pest_phenology(trap_df, pest = "OLFF", weather_source = "cimis_csv",
                        cimis_csv_path = tempfile(fileext = ".csv")),
    "2024-04-08"
  )
})

test_that("invalid CIMIS date format errors clearly", {
  dates <- seq(as.Date("2024-04-01"), as.Date("2024-04-10"), by = "day")
  csv_path <- make_cimis_csv(dates, tmax = 85, tmin = 55, date_fmt = "%Y-%m-%d")
  
  trap_df <- data.frame(date = c("4/1/2024", "4/8/2024"), trap_counts = c(0, 3))
  
  expect_error(
    calc_pest_phenology(trap_df, pest = "OLFF", weather_source = "cimis_csv",
                        cimis_csv_path = csv_path),
    "Could not parse|MM/DD/YYYY"
  )
})

test_that("missing pest and missing custom thresholds errors", {
  trap_df <- data.frame(date = c("4/1/2024", "4/8/2024"), trap_counts = c(0, 3))
  expect_error(
    calc_pest_phenology(trap_df, weather_source = "cimis_csv",
                        cimis_csv_path = tempfile(fileext = ".csv")),
    "custom_lower.*custom_upper|pest.*code"
  )
})

test_that("listed pest with only one custom threshold falls back to database value for the other", {
  dates <- seq(as.Date("2024-04-01"), as.Date("2024-04-10"), by = "day")
  csv_path <- make_cimis_csv(dates, tmax = 85, tmin = 55)
  trap_df <- data.frame(date = c("4/1/2024", "4/8/2024"), trap_counts = c(0, 3))
  
  result <- expect_message(
    calc_pest_phenology(trap_df, pest = "OLFF", weather_source = "cimis_csv",
                        cimis_csv_path = csv_path, custom_lower = 65),
    "Lower = 65"
  )
})

test_that("unknown pest code without custom thresholds errors", {
  trap_df <- data.frame(date = c("4/1/2024", "4/8/2024"), trap_counts = c(0, 3))
  expect_error(
    calc_pest_phenology(trap_df, pest = "NOT_A_PEST_LISTED", weather_source = "cimis_csv",
                        cimis_csv_path = tempfile(fileext = ".csv")),
    "not found"
  )
})

test_that("unknown pest code with only one custom threshold still errors", {
  trap_df <- data.frame(date = c("4/1/2024", "4/8/2024"), trap_counts = c(0, 3))
  expect_error(
    calc_pest_phenology(trap_df, pest = "NOT_A_PEST_LISTED", weather_source = "cimis_csv",
                        cimis_csv_path = tempfile(fileext = ".csv"), custom_lower = 50),
    "not found"
  )
})

test_that("unknown pest code with custom thresholds succeeds and computes expected DD", {
  dates <- seq(as.Date("2024-04-01"), as.Date("2024-04-10"), by = "day")
  csv_path <- make_cimis_csv(dates, tmax = 85, tmin = 55)
  trap_df <- data.frame(date = c("4/1/2024", "4/8/2024"), trap_counts = c(0, 3))
  
  result <- calc_pest_phenology(
    trap_df, pest = "NOT_A_PEST_LISTED", weather_source = "cimis_csv",
    cimis_csv_path = csv_path, custom_lower = 50, custom_upper = 90,
    custom_pest_label = "Test Pest"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(attr(result, "pest_label"), "Test Pest (NOT_A_PEST_LISTED)")
  expect_equal(result$cumulative_dd[result$date == as.Date("2024-04-01")], 20)
})

test_that("unknown pest code without custom_pest_label falls back to the raw code", {
  dates <- seq(as.Date("2024-04-01"), as.Date("2024-04-10"), by = "day")
  csv_path <- make_cimis_csv(dates, tmax = 85, tmin = 55)
  trap_df <- data.frame(date = c("4/1/2024", "4/8/2024"), trap_counts = c(0, 3))
  
  result <- calc_pest_phenology(
    trap_df, pest = "not_a_pest_listed", weather_source = "cimis_csv",
    cimis_csv_path = csv_path, custom_lower = 50, custom_upper = 90
  )
  expect_equal(attr(result, "pest_label"), "NOT_A_PEST_LISTED")
})

test_that("missing required CIMIS columns error with column names listed", {
  dates <- seq(as.Date("2024-04-01"), as.Date("2024-04-10"), by = "day")
  df <- data.frame(Date = format(dates, "%m/%d/%Y"), `Max Air Temp (F)` = 85, check.names = FALSE)
  csv_path <- tempfile(fileext = ".csv")
  write.csv(df, csv_path, row.names = FALSE)
  
  trap_df <- data.frame(date = c("4/1/2024", "4/8/2024"), trap_counts = c(0, 3))
  expect_error(
    calc_pest_phenology(trap_df, pest = "OLFF", weather_source = "cimis_csv", cimis_csv_path = csv_path),
    "Min.Air.Temp..F."
  )
})

test_that("missing trap_counts column errors rather than failing silently", {
  trap_df <- data.frame(date = c("4/1/2024", "4/8/2024"))
  dates <- seq(as.Date("2024-04-01"), as.Date("2024-04-10"), by = "day")
  csv_path <- make_cimis_csv(dates, tmax = 85, tmin = 55)
  
  expect_error(
    calc_pest_phenology(trap_df, pest = "OLFF", weather_source = "cimis_csv", cimis_csv_path = csv_path)
  )
})

test_that("cimis_csv path requires cimis_csv_path argument", {
  trap_df <- data.frame(date = c("4/1/2024", "4/8/2024"), trap_counts = c(0, 3))
  expect_error(
    calc_pest_phenology(trap_df, pest = "OLFF", weather_source = "cimis_csv"),
    "cimis_csv_path"
  )
})

test_that("open_meteo path requires lat/lon", {
  trap_df <- data.frame(date = c("4/1/2024", "4/8/2024"), trap_counts = c(0, 3))
  expect_error(
    calc_pest_phenology(trap_df, pest = "OLFF", weather_source = "open_meteo"),
    "Latitude and longitude"
  )
})

test_that("open_meteo path attempts a live fetch when lat/lon supplied (network required)", {
  skip_if_offline()
  skip_on_cran()
  trap_df <- data.frame(
    date = c(format(Sys.Date() - 20, "%m/%d/%Y"), format(Sys.Date() - 5, "%m/%d/%Y")),
    trap_counts = c(0, 3)
  )
  result <- tryCatch(
    calc_pest_phenology(trap_df, pest = "OLFF", weather_source = "open_meteo",
                        lat = 36.3302, lon = -119.2921),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    skip(paste("Open-Meteo unreachable in this environment:", conditionMessage(result)))
  }
  expect_s3_class(result, "data.frame")
  expect_true(all(c("cumulative_dd", "cumulative_dd_from_biofix") %in% names(result)))
})

test_that("daymet path requires lat/lon and rejects current-year end dates", {
  trap_df <- data.frame(
    date = c(format(Sys.Date() - 10, "%m/%d/%Y"), format(Sys.Date(), "%m/%d/%Y")),
    trap_counts = c(0, 3)
  )
  expect_error(
    calc_pest_phenology(trap_df, pest = "OLFF", weather_source = "daymet"),
    "Latitude and longitude"
  )
  expect_error(
    calc_pest_phenology(trap_df, pest = "OLFF", weather_source = "daymet", lat = 36.33, lon = -119.29),
    "previous calendar year"
  )
})

test_that("invalid weather_source value errors", {
  trap_df <- data.frame(date = c("4/1/2024", "4/8/2024"), trap_counts = c(0, 3))
  expect_error(
    calc_pest_phenology(trap_df, pest = "OLFF", weather_source = "not_a_source"),
    "Invalid weather_source"
  )
})

test_that("no positive trap counts triggers the biofix error, not a silent result", {
  dates <- seq(as.Date("2024-04-01"), as.Date("2024-04-10"), by = "day")
  csv_path <- make_cimis_csv(dates, tmax = 85, tmin = 55)
  trap_df <- data.frame(date = c("4/1/2024", "4/8/2024"), trap_counts = c(0, 0))
  
  expect_error(
    calc_pest_phenology(trap_df, pest = "OLFF", weather_source = "cimis_csv", cimis_csv_path = csv_path),
    "No positive trap counts"
  )
})

test_that("pests with no upper threshold (MRB, PBA, ZMA) calculate DD without an upper cutoff, not all-zero", {
  no_upper_pests <- c("MRB", "PBA", "ZMA")
  for (code in no_upper_pests) {
    thresh_row <- TrackTrap::pest_thresholds[TrackTrap::pest_thresholds$pest_code == code, ]
    expect_true(is.na(thresh_row$upper_thresh),
                info = sprintf("Expected %s to have NA upper_thresh in the database", code))
    
    dates <- seq(as.Date("2024-04-01"), as.Date("2024-04-10"), by = "day")
    csv_path <- make_cimis_csv(dates, tmax = 95, tmin = 70)
    trap_df <- data.frame(date = c("4/1/2024", "4/10/2024"), trap_counts = c(1, 0))
    
    result <- calc_pest_phenology(
      trap_df, pest = code, weather_source = "cimis_csv", cimis_csv_path = csv_path
    )
    
    expect_false(all(result$cumulative_dd_from_biofix == 0),
                 info = sprintf("%s: cumulative DD incorrectly all zero -- NA upper_thresh bug present", code))
    expect_false(anyNA(result$cumulative_dd),
                 info = sprintf("%s: cumulative_dd contains NA -- upper_thresh NA propagated incorrectly", code))
    expect_false(attr(result, "upper_thresh_defined"),
                 info = sprintf("%s: upper_thresh_defined attribute should be FALSE", code))
  }
})

test_that("uncapped degree-days for NA upper_thresh equal avg_temp - lower_thresh with no ceiling", {
  dates <- seq(as.Date("2024-04-01"), as.Date("2024-04-05"), by = "day")
  csv_path <- make_cimis_csv(dates, tmax = 120, tmin = 100)
  trap_df <- data.frame(date = c("4/1/2024", "4/5/2024"), trap_counts = c(1, 0))
  
  result <- calc_pest_phenology(
    trap_df, pest = "ZMA", weather_source = "cimis_csv", cimis_csv_path = csv_path
  )
  
  zma_lower <- TrackTrap::pest_thresholds$lower_thresh[TrackTrap::pest_thresholds$pest_code == "ZMA"]
  expected_daily_dd <- max(((120 + 100) / 2) - zma_lower, 0)
  
  day1_dd <- result$cumulative_dd[result$date == as.Date("2024-04-01")]
  expect_equal(day1_dd, expected_daily_dd, tolerance = 1e-6)
})

test_that("custom_upper = NA also triggers uncapped calculation for an unlisted pest", {
  dates <- seq(as.Date("2024-04-01"), as.Date("2024-04-05"), by = "day")
  csv_path <- make_cimis_csv(dates, tmax = 100, tmin = 80)
  trap_df <- data.frame(date = c("4/1/2024", "4/5/2024"), trap_counts = c(1, 0))
  
  result <- calc_pest_phenology(
    trap_df, pest = "NOT_LISTED_PEST", weather_source = "cimis_csv", cimis_csv_path = csv_path,
    custom_lower = 50, custom_upper = NA_real_, custom_pest_label = "Test Pest"
  )
  
  expect_false(anyNA(result$cumulative_dd))
  expect_false(all(result$cumulative_dd == 0))
  expect_false(attr(result, "upper_thresh_defined"))
})

test_that("pests with a defined upper threshold are still capped correctly", {
  dates <- seq(as.Date("2024-04-01"), as.Date("2024-04-05"), by = "day")
  csv_path <- make_cimis_csv(dates, tmax = 120, tmin = 100)
  trap_df <- data.frame(date = c("4/1/2024", "4/5/2024"), trap_counts = c(1, 0))
  
  result <- calc_pest_phenology(
    trap_df, pest = "OLFF", weather_source = "cimis_csv", cimis_csv_path = csv_path
  )
  
  olff_lower <- TrackTrap::pest_thresholds$lower_thresh[TrackTrap::pest_thresholds$pest_code == "OLFF"]
  olff_upper <- TrackTrap::pest_thresholds$upper_thresh[TrackTrap::pest_thresholds$pest_code == "OLFF"]
  expected_daily_dd <- olff_upper - olff_lower
  
  day1_dd <- result$cumulative_dd[result$date == as.Date("2024-04-01")]
  expect_equal(day1_dd, expected_daily_dd, tolerance = 1e-6)
  expect_true(attr(result, "upper_thresh_defined"))
})