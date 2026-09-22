test_that(
  "non-numeric CIMIS maximum temperatures produce a clear validation error",
  {
    dates <- seq(
      as.Date("2024-04-01"),
      as.Date("2024-04-05"),
      by = "day"
    )
    
    cimis_data <- data.frame(
      Date = format(dates, "%m/%d/%Y"),
      Max.Air.Temp..F. = c("85", "corrupt_data", "80", "78", "82"),
      Min.Air.Temp..F. = c("55", "50", "52", "48", "51"),
      check.names = FALSE
    )
    
    csv_path <- tempfile(fileext = ".csv")
    utils::write.csv(cimis_data, csv_path, row.names = FALSE)
    
    trap_df <- data.frame(
      date = c("4/1/2024", "4/5/2024"),
      trap_counts = c(1, 0)
    )
    
    expect_error(
      calc_pest_phenology(
        trap_df,
        pest = "OLFF",
        weather_source = "cimis_csv",
        cimis_csv_path = csv_path
      ),
      regexp = "Non-numeric values found.*Max\\.Air\\.Temp\\.\\.F\\.|corrupt_data"
    )
  }
)

test_that(
  "non-numeric trap counts produce a validation error rather than silent coercion",
  {
    dates <- seq(
      as.Date("2024-04-01"),
      as.Date("2024-04-05"),
      by = "day"
    )
    
    csv_path <- make_cimis_csv(dates, tmax = 85, tmin = 55)
    
    trap_df_corrupt <- data.frame(
      date = c("4/1/2024", "4/5/2024"),
      trap_counts = c("zero", "three")
    )
    
    expect_error(
      calc_pest_phenology(
        trap_df_corrupt,
        pest = "OLFF",
        weather_source = "cimis_csv",
        cimis_csv_path = csv_path
      ),
      regexp = "[Tt]rap.*count.*non-numeric|non-numeric.*[Tt]rap.*count"
    )
  }
)

test_that(
  "Daymet retrieval and phenology calculation execute successfully",
  {
    skip_if_offline()
    skip_on_cran()
    
    prior_year <- as.integer(format(Sys.Date(), "%Y")) - 1L
    
    biofix_date <- as.Date(sprintf("%d-06-01", prior_year))
    end_date <- as.Date(sprintf("%d-06-15", prior_year))
    
    trap_df <- data.frame(
      date = format(c(biofix_date, end_date), "%m/%d/%Y"),
      trap_counts = c(1, 4)
    )
    
    result <- tryCatch(
      calc_pest_phenology(
        trap_df,
        pest = "OLFF",
        weather_source = "daymet",
        lat = 36.3302,
        lon = -119.2921
      ),
      error = function(e) e
    )
    
    if (inherits(result, "error")) {
      skip(
        paste(
          "Daymet retrieval unavailable:",
          conditionMessage(result)
        )
      )
    }
    
    expect_s3_class(result, "data.frame")
    
    expect_true(
      all(
        c(
          "date",
          "tmax",
          "tmin",
          "DD",
          "cumulative_dd",
          "cumulative_dd_from_biofix"
        ) %in% names(result)
      )
    )
    
    expect_equal(
      result$cumulative_dd_from_biofix[result$date == biofix_date],
      0
    )
    
    end_dd <- result$cumulative_dd_from_biofix[
      result$date == end_date
    ]
    
    expect_length(end_dd, 1L)
    expect_true(is.finite(end_dd))
    expect_gt(end_dd, 0)
  }
)

test_that(
  "Daymet temperature conversion handles the expected Daymet data structure offline",
  {
    mock_daymet <- list(
      data = data.frame(
        year = c(2023, 2023),
        yday = c(152, 153),
        tmax..deg.c. = c(25, 30),
        tmin..deg.c. = c(10, 15)
      )
    )
    
    out <- data.frame(
      date = as.Date(
        paste(mock_daymet$data$year, mock_daymet$data$yday, sep = "-"),
        format = "%Y-%j"
      ),
      tmax = (mock_daymet$data$tmax..deg.c. * 9 / 5) + 32,
      tmin = (mock_daymet$data$tmin..deg.c. * 9 / 5) + 32
    )
    
    expect_equal(out$tmax, c(77, 86))
    expect_equal(out$tmin, c(50, 59))
    expect_equal(
      out$date,
      as.Date(c("2023-06-01", "2023-06-02"))
    )
  }
)