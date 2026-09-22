test_that("pest_thresholds has the required structure", {
  expect_s3_class(pest_thresholds, "data.frame")
  
  expect_equal(nrow(pest_thresholds), 90L)
  
  expect_setequal(
    names(pest_thresholds),
    c(
      "pest_code",
      "pest_name",
      "lower_thresh",
      "upper_thresh",
      "flight_interval_dd"
    )
  )
})

test_that("pest_thresholds has valid required identifiers and lower thresholds", {
  expect_true(all(nzchar(pest_thresholds$pest_code)))
  expect_true(all(nzchar(pest_thresholds$pest_name)))
  
  expect_equal(anyDuplicated(pest_thresholds$pest_code), 0L)
  
  expect_true(all(!is.na(pest_thresholds$lower_thresh)))
  expect_true(all(is.numeric(pest_thresholds$lower_thresh)))
  expect_true(all(pest_thresholds$lower_thresh > 0))
})

test_that("defined upper thresholds exceed lower thresholds", {
  defined_upper <- !is.na(pest_thresholds$upper_thresh)
  
  expect_true(
    all(
      pest_thresholds$upper_thresh[defined_upper] >
        pest_thresholds$lower_thresh[defined_upper]
    )
  )
})

test_that("pests without an upper threshold remain NA", {
  no_upper_codes <- c("MRB", "PBA", "ZMA", "GWB")
  
  no_upper_rows <- pest_thresholds[
    pest_thresholds$pest_code %in% no_upper_codes,
    ,
    drop = FALSE
  ]
  
  expect_equal(nrow(no_upper_rows), length(no_upper_codes))
  expect_true(all(is.na(no_upper_rows$upper_thresh)))
})

test_that("pest codes are uppercase and contain only letters and numbers", {
  expect_true(
    all(grepl("^[A-Z0-9]+$", pest_thresholds$pest_code))
  )
})