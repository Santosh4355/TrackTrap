test_that("calc_pest_phenology detects biofix correctly on dummy data", {
  synthetic_weather <- data.frame(
    Date = format(seq(as.Date("2024-04-01"), as.Date("2024-05-15"), by = "day"), "%m/%d/%Y"),
    Max.Air.Temp..F. = 75,
    Min.Air.Temp..F. = 45,
    check.names = FALSE
  )
  tmp_csv <- tempfile(fileext = ".csv")
  write.csv(synthetic_weather, tmp_csv, row.names = FALSE)
  
  trap_df <- data.frame(
    date = as.Date(c("2024-04-01", "2024-04-08", "2024-04-15", "2024-04-22")),
    trap_counts = c(0, 0, 3, 9)
  )
  
  result <- calc_pest_phenology(
    trap_df, pest = "OLFF",
    weather_source = "cimis_csv",
    cimis_csv_path = tmp_csv
  )
  
  expect_s3_class(result, "data.frame")
  expect_true("cumulative_dd_from_biofix" %in% names(result))
  expect_equal(min(result$date[result$trap_counts > 0]), as.Date("2024-04-15"))
})