# TrackTrap 1.1.0

## Bug fixes

* Corrected the vignette's description of location handling for the
  `weather_source = "cimis_csv"` pathway. `calc_pest_phenology()` does not
  read station location from the CIMIS CSV; it extracts only `Date`,
  `Max.Air.Temp..F.`, and `Min.Air.Temp..F.`. `lat`/`lon` arguments are
  ignored in this mode. The corresponding runtime message in
  `calc_pest_phenology()` was also corrected to accurately reflect this
  behavior. Users who want a location title on plots produced from
  `cimis_csv` data should provide `lat`/`lon` directly to
  `plot_trap_phenology()`.
* Replaced a hardcoded, machine-specific file path in the CIMIS vignette with
  a portable `system.file()` reference to the bundled example dataset, and
  enabled full execution of all vignette code chunks (previously
  `eval = FALSE`) so the vignette is verified to run end-to-end on package
  build.
  * Fixed `calc_pest_phenology()` silently zeroing out cumulative degree-days
  for pests with no published upper developmental threshold (Mint Root
  Borer, Pecan Nut Casebearer, Zimmerman Pine Moth, Grape Root Borer).
  `pmin(x, NA)` previously propagated `NA` through the entire degree-day
  series; these pests now calculate degree-days without an upper cutoff.
  * Corrected the trap data input format documentation (M/D/YYYY or MM/DD/YYYY, not
  YYYY-MM-DD).

## Dependency changes

* Removed `degday` as a package dependency (`DESCRIPTION` and `NAMESPACE`).
  TrackTrap's growing degree-day calculation is implemented directly using
  base R vectorized operations and does not call `degday`'s computational
  functions.


## Testing

* Substantially expanded the `testthat` suite for `calc_pest_phenology()`:
  - Confirmed accumulated degree-days equal zero at the biofix date.
  - Added tests for missing pest thresholds (both listed and unlisted pest
    codes), invalid trap and CIMIS date formats, and missing required input
    columns.
  - Added tests for upper-threshold capping and lower-threshold flooring of
    daily degree-days.
  - Extended coverage across all three weather-input pathways
    (`cimis_csv`, `open_meteo`, `daymet`), with network-dependent tests
    gated by `skip_if_offline()` and `skip_on_cran()`.
    
* Added `upper_thresh_defined` attribute to `calc_pest_phenology()` output,
  indicating whether an upper developmental threshold was applied.
  
* Clarified console messaging to distinguish an undefined upper threshold
  from a real numeric value.
    

## Data

* Revised `pest_thresholds` reference data from the current source
  spreadsheet, correcting several threshold and flight-interval values and
  adding two new pest entries (Giant whitefly, `GWF`; Peach Fruit Fly,
  `PFF`).

# TrackTrap 1.0.1

Prior CRAN release.

