test_that("pest_thresholds has expected structure", {
  expect_s3_class(pest_thresholds, "data.frame")
  expect_equal(nrow(pest_thresholds), 90)
  expect_setequal(
    names(pest_thresholds),
    c("pest_code", "pest_name", "lower_thresh", "upper_thresh",
      "flight_interval_dd")
  )
  expect_true(all(!is.na(pest_thresholds$lower_thresh)))
  expect_equal(anyDuplicated(pest_thresholds$pest_code), 0)
})