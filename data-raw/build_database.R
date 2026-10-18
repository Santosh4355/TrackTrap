# data-raw/build_database.R
# Run from the TrackTrap package root.

master <- utils::read.csv(
  "data-raw/pest_thresholds_master.csv",
  na.strings = c("", "NA"),
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8",
  encoding = "UTF-8"
)

required <- c(
  "pest_code",
  "pest_name",
  "scientific_name",
  "lower_thresh",
  "lower_source",
  "upper_thresh",
  "upper_source",
  "thermal_requirement_dd",
  "thermal_requirement_source",
  "thermal_requirement_endpoint"
)

missing_columns <- setdiff(required, names(master))

if (length(missing_columns)) {
  stop(
    "Missing required columns: ",
    paste(missing_columns, collapse = ", "),
    call. = FALSE
  )
}

stopifnot(
  nrow(master) == 90L,
  !anyDuplicated(names(master))
)

# Validate and explicitly mark UTF-8 text before saving.
for (field in names(master)) {
  values <- master[[field]]
  
  if (is.character(values)) {
    defined <- !is.na(values)
    
    if (any(!validUTF8(values[defined]))) {
      stop(
        "Invalid UTF-8 text in column: ",
        field,
        call. = FALSE
      )
    }
    
    Encoding(values) <- "UTF-8"
    master[[field]] <- values
  }
}

# Validate identifiers.
for (field in c("pest_code", "pest_name")) {
  values <- master[[field]]
  
  stopifnot(
    is.character(values),
    !anyNA(values),
    all(nzchar(trimws(values))),
    all(values == trimws(values))
  )
}

stopifnot(
  !anyDuplicated(master$pest_code),
  all(grepl("^[A-Z0-9]+$", master$pest_code))
)

# Convert numeric parameters without silently discarding invalid text.
for (field in c(
  "lower_thresh",
  "upper_thresh",
  "thermal_requirement_dd"
)) {
  raw <- trimws(as.character(master[[field]]))
  missing <- is.na(raw) | raw %in% c("", "NA")
  
  values <- suppressWarnings(as.numeric(raw))
  invalid <- !missing & !is.finite(values)
  
  if (any(invalid)) {
    stop(
      "Invalid numeric values in ", field,
      " for pest codes: ",
      paste(master$pest_code[invalid], collapse = ", "),
      call. = FALSE
    )
  }
  
  values[missing] <- NA_real_
  master[[field]] <- values
}

# Check numerical consistency.
paired <- !is.na(master$lower_thresh) &
  !is.na(master$upper_thresh)

stopifnot(
  all(master$upper_thresh[paired] > master$lower_thresh[paired]),
  all(
    is.na(master$thermal_requirement_dd) |
      master$thermal_requirement_dd > 0
  )
)

# Every populated parameter must have a source.
for (pair in list(
  c("lower_thresh", "lower_source"),
  c("upper_thresh", "upper_source"),
  c("thermal_requirement_dd", "thermal_requirement_source")
)) {
  populated <- !is.na(master[[pair[1]]])
  sources <- master[[pair[2]]][populated]
  
  if (anyNA(sources) || any(!nzchar(trimws(sources)))) {
    stop(
      "Missing source for a populated parameter: ",
      pair[1],
      call. = FALSE
    )
  }
}

# Every populated thermal requirement must identify its endpoint.
populated <- !is.na(master$thermal_requirement_dd)
endpoints <- master$thermal_requirement_endpoint[populated]

if (anyNA(endpoints) || any(!nzchar(trimws(endpoints)))) {
  stop(
    "A populated thermal requirement lacks its developmental endpoint.",
    call. = FALSE
  )
}

pest_thresholds <- master

# Recreate the standalone script, retaining column/variable names.
# Unicode escapes preserve characters in portable R source.
script_path <- "data-raw/pest_thresholds.R"
con <- file(script_path, "w", encoding = "UTF-8")

tryCatch(
  {
    for (field in names(pest_thresholds)) {
      writeLines(
        paste0("`", field, "` <-"),
        con
      )
      
      dput(
        pest_thresholds[[field]],
        con,
        control = c("keepNA", "keepInteger", "niceNames", "hexNumeric")
      )
    }
    
    writeLines(
      paste0(
        "pest_thresholds <- data.frame(\n  ",
        paste(
          paste0("`", names(pest_thresholds), "`"),
          collapse = ",\n  "
        ),
        ",\n  stringsAsFactors = FALSE,\n",
        "  check.names = FALSE\n)"
      ),
      con
    )
    
    writeLines(
      c(
        "",
        "for (field in names(pest_thresholds)) {",
        "  if (is.character(pest_thresholds[[field]])) {",
        '    Encoding(pest_thresholds[[field]]) <- "UTF-8"',
        "  }",
        "}"
      ),
      con
    )
  },
  finally = close(con)
)

# Save package data with declared string encodings.
dir.create("data", showWarnings = FALSE)

save(
  pest_thresholds,
  file = "data/pest_thresholds.rda",
  compress = "xz",
  version = 2
)

# Export the same object for supplementary documentation.
dir.create("supplementary", showWarnings = FALSE)

utils::write.csv(
  pest_thresholds,
  "supplementary/pest_thresholds.csv",
  row.names = FALSE,
  na = "NA",
  fileEncoding = "UTF-8"
)

# Verify the saved package object.
package_copy <- new.env(parent = baseenv())

load(
  "data/pest_thresholds.rda",
  envir = package_copy
)

stopifnot(
  identical(package_copy$pest_thresholds, pest_thresholds)
)

# Verify the supplementary CSV.
check <- utils::read.csv(
  "supplementary/pest_thresholds.csv",
  na.strings = "NA",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8",
  encoding = "UTF-8"
)

stopifnot(
  isTRUE(all.equal(
    package_copy$pest_thresholds,
    check,
    check.attributes = FALSE
  ))
)

# Verify that the generated standalone R script recreates the data.
script_copy <- new.env(parent = baseenv())

sys.source(script_path, envir = script_copy)

stopifnot(
  isTRUE(all.equal(
    script_copy$pest_thresholds,
    pest_thresholds,
    check.attributes = FALSE
  ))
)

message(
  "Package database, supplementary CSV, and standalone R script ",
  "generated from one master and compared successfully."
)