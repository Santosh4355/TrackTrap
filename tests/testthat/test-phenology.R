test_that("matching thermal references display the database endpoint", {
  pheno <- data.frame(
    cumulative_dd_from_biofix = c(0, 100, 250),
    trap_counts = c(1, 4, 2)
  )
  
  attr(pheno, "calculation_metadata") <- list(
    database_thermal_requirement_dd = 100,
    database_thermal_requirement_endpoint = "Egg-to-adult development"
  )
  
  p <- plot_trap_phenology(
    pheno,
    custom_thermal_requirement = 100,
    repeat_markers = TRUE
  )
  
  expect_s3_class(p, "ggplot")
  
  expect_match(
    p$labels$subtitle,
    "100.0",
    fixed = TRUE
  )
  
  expect_match(
    p$labels$subtitle,
    "Egg-to-adult development (database reference)",
    fixed = TRUE
  )
  
  expect_match(
    p$labels$caption,
    "Dotted lines mark multiples",
    fixed = TRUE
  )
}) 

test_that("custom values do not inherit an unrelated database endpoint", {
  pheno <- data.frame(
    cumulative_dd_from_biofix = c(0, 100, 250),
    trap_counts = c(1, 4, 2)
  )
  
  attr(pheno, "calculation_metadata") <- list(
    database_thermal_requirement_dd = 100,
    database_thermal_requirement_endpoint = "Egg-to-adult development"
  )
  
  p <- plot_trap_phenology(
    pheno,
    custom_thermal_requirement = 150
  )
  
  expect_match(
    p$labels$subtitle,
    "developmental endpoint not specified",
    fixed = TRUE
  )
  
  expect_false(grepl(
    "Egg-to-adult",
    p$labels$subtitle,
    fixed = TRUE
  ))
  
  expect_match(
    p$labels$caption,
    "The dotted line marks this reference value",
    fixed = TRUE
  )
})

test_that("custom references work without database metadata", {
  pheno <- data.frame(
    cumulative_dd_from_biofix = c(0, 100, 250),
    trap_counts = c(1, 4, 2)
  )
  
  p <- plot_trap_phenology(
    pheno,
    custom_thermal_requirement = 100
  )
  
  expect_match(
    p$labels$subtitle,
    "developmental endpoint not specified",
    fixed = TRUE
  )
  
  expect_match(
    p$labels$caption,
    "Thermal requirement = accumulated heat",
    fixed = TRUE
  )
})

test_that("database availability alone does not select thermal markers", {
  pheno <- data.frame(
    cumulative_dd_from_biofix = c(0, 100, 250),
    trap_counts = c(1, 4, 2)
  )
  
  attr(pheno, "calculation_metadata") <- list(
    database_thermal_requirement_dd = 100,
    database_thermal_requirement_endpoint = "Egg-to-adult development"
  )
  
  p <- plot_trap_phenology(pheno)
  
  expect_null(p$labels$subtitle)
  expect_null(p$labels$caption)
})

test_that("thermal references selected during calculation are explained", {
  dates <- as.Date("2024-04-01") + 0:2
  
  trap <- data.frame(
    date = dates[c(1, 3)],
    trap_counts = c(1, 4)
  )
  
  weather <- data.frame(
    Date = dates,
    tmin = c(55, 55, 55),
    tmax = c(85, 85, 85)
  )
  
  pheno <- calc_pest_phenology(
    trap_data = trap,
    pest = NULL,
    weather_data = weather,
    weather_provenance = list(
      data_type = "Synthetic test temperatures"
    ),
    custom_lower = 50,
    upper_cutoff = "none",
    custom_thermal_requirement = 30
  )
  

  p <- plot_trap_phenology(pheno)
  
  expect_match(
    p$labels$subtitle,
    "30.0",
    fixed = TRUE
  )
  
  expect_match(
    p$labels$subtitle,
    "developmental endpoint not specified",
    fixed = TRUE
  )
  
  expect_match(
    p$labels$caption,
    "The dotted line marks this reference value",
    fixed = TRUE
  )
})