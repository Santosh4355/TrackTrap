test_that("current bundled database contains identifiers, parameters and sources", {
  expect_s3_class(pest_thresholds, "data.frame")
  expect_equal(nrow(pest_thresholds), 90L)
  required <- c("pest_code", "pest_name", "scientific_name", "lower_thresh",
                "upper_thresh", "thermal_requirement_dd", "lower_source", "upper_source",
                "thermal_requirement_source", "thermal_requirement_endpoint")
  expect_true(all(required %in% names(pest_thresholds)))
  for (field in c("pest_code", "pest_name", "scientific_name")) {
    values <- pest_thresholds[[field]]
    expect_type(values, "character")
    expect_false(anyNA(values))
    expect_true(all(!is.na(values) & nzchar(trimws(values))))
  }
  expect_equal(anyDuplicated(pest_thresholds$pest_code), 0L)
  expect_true(all(grepl("^[A-Z0-9]+$", pest_thresholds$pest_code)))
  expect_true(all(c("PC", "TBW", "BAB", "CSB", "GWB", "FAW") %in%
                    pest_thresholds$pest_code))
})

test_that("defined parameters are finite and internally consistent", {
  for (field in c("lower_thresh", "upper_thresh", "thermal_requirement_dd")) {
    values <- pest_thresholds[[field]]
    expect_true(is.numeric(values), info = field)
    expect_false(any(is.nan(values)), info = field)
    expect_true(all(is.finite(values[!is.na(values)])), info = field)
  }
  paired <- !is.na(pest_thresholds$lower_thresh) & !is.na(pest_thresholds$upper_thresh)
  expect_true(all(pest_thresholds$upper_thresh[paired] > pest_thresholds$lower_thresh[paired]))
  values <- pest_thresholds$thermal_requirement_dd
  expect_true(all(values[!is.na(values)] > 0))
  if ("flight_interval_dd" %in% names(pest_thresholds))
    expect_equal(pest_thresholds$flight_interval_dd, values)
})

test_that("populated parameters carry source and endpoint information", {
  for (pair in list(c("lower_thresh", "lower_source"),
                    c("upper_thresh", "upper_source"),
                    c("thermal_requirement_dd", "thermal_requirement_source"))) {
    populated <- !is.na(pest_thresholds[[pair[1]]])
    sources <- pest_thresholds[[pair[2]]][populated]
    expect_false(anyNA(sources), info = pair[2])
    expect_true(all(!is.na(sources) & nzchar(trimws(sources))), info = pair[2])
  }
  endpoint <- pest_thresholds$thermal_requirement_endpoint[
    !is.na(pest_thresholds$thermal_requirement_dd)]
  expect_true(all(!is.na(endpoint) & nzchar(trimws(endpoint))))
})
