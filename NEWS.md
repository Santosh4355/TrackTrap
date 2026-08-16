# TrackTrap 1.0.1

## Bug fixes and documentation

1. Test coverage. Replaced the placeholder test (`expect_equal(2 * 2, 4)`) with a real test suite covering `calc_pest_phenology()` (biofix identification on a dummy trap dataset, using a manual test CIMIS CSV) and `pest_thresholds` (row count and column-name checks). `R CMD check` now demonstrates actual package behavior rather than trivially passing.

2. Console feedback consistency. `fetch_open_meteo()` now displays a success message on completion ("Open-Meteo fetch complete: N days retrieved."), matching the confirmation users already saw from the Daymet path. The Daymet path itself calls `daymetr::download_daymet(..., silent = TRUE)` and prints its own consistently formatted message instead of relying on daymetr's internal verbosity.

3. Removed unused `pest_thresholds` columns that were intended for future use due to limited accessibility of credible sources. `predictive_biofix_dd`, and `start_date` have all been dropped from the dataset. The Rd has been updated to document only the columns the package actually uses: `pest_code`, `pest_name`, `lower_thresh`, `upper_thresh`, and `flight_interval_dd` (used to draw flight-generation lines in `plot_trap_phenology()`). Source citation information has been moved to the dataset's `@source` documentation field rather than kept as a per-row column.

4. `pest_thresholds` dataset revised to include 90 different major pests from the United States of America (USA).

5. `cimis_csv_path` documentation and validation. The manual CIMIS workflow was previously documented only as "Path to the CIMIS csv file." Now documents: required column names after `read.csv()` mangling (`Date`, `Max.Air.Temp..F.`, `Min.Air.Temp..F.`), required `MM/DD/YYYY` date format, that `lat`/`lon` are ignored when `cimis_csv_path` is supplied, and that `weather_source` must be the exact string `"cimis_csv"`. Added explicit input validation so mismatched arguments, missing columns, or incomprehensible dates raise informative errors instead of propagating silent `NA`s. Added a new vignette, `cimis-manual-workflow`, walking through the complete manual CIMIS workflow end to end.

6. The package now accepts information to calculate GDD for unlisted pests in the database, or if users intend to use custom inputs for the GDD calculation and visualization.

7. Plot titles now include year and coordinates. `plot_trap_phenology()` titles now display the trapping year and latitude/longitude of the site, pulled automatically from the attributes `calc_pest_phenology()` attaches to its output.

# TrackTrap 1.0.0

Prior CRAN release.

