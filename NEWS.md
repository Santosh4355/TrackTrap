# TrackTrap 1.1.0

## CIMIS CSV handling

* Corrected the vignette documentation for `weather_source = "cimis_csv"`.

  `calc_pest_phenology()` does not read station location metadata from a
  CIMIS CSV. In this mode, it extracts only `Date`, `Max.Air.Temp..F.`, and
  `Min.Air.Temp..F.`. The `lat` and `lon` arguments are ignored by
  `calc_pest_phenology()` when `weather_source = "cimis_csv"`.

  Users who want location information displayed on phenology plots generated
  from CIMIS CSV input should supply `lat` and `lon` directly to
  `plot_trap_phenology()`.

* Updated the `calc_pest_phenology()` console message for the `cimis_csv`
  pathway to accurately describe this location-handling behavior.

* Replaced a hard-coded, machine-specific CIMIS vignette path with a portable
  `system.file()` reference to the package's bundled example data.

* Enabled end-to-end execution of all vignette code chunks. Previously, code
  chunks were disabled with `eval = FALSE`; they are now evaluated during
  package build.

## Bug fixes

* Fixed degree-day calculation for pests with no published upper developmental
  threshold:

  - Mint Root Borer (`MRB`)
  - Pecan Nut Casebearer (`PBA`)
  - Zimmerman Pine Moth (`ZMA`)
  - Grape Root Borer (`GWB`)

  Previously, upper-threshold capping applied `pmin(x, NA)`, which propagated
  `NA` through daily degree-day values and resulted in invalid cumulative
  degree-day calculations. These pests now accumulate degree-days using the
  lower developmental threshold without upper-temperature capping.

* Added an `upper_thresh_defined` attribute to
  `calc_pest_phenology()` output. The attribute records whether an upper
  developmental threshold was applied during degree-day calculation.

* Clarified `calc_pest_phenology()` console messages to distinguish an
  undefined upper developmental threshold from a numeric upper threshold.

* Corrected trap-data date-format documentation. Supported trap-date formats
  are `M/D/YYYY` and `MM/DD/YYYY`; `YYYY-MM-DD` is not currently accepted.

## Dependency changes

* Removed `degday` from `DESCRIPTION` and `NAMESPACE`.

  TrackTrap calculates growing degree-days internally using base R vectorized
  operations.

## Testing

* Substantially expanded `testthat` coverage for `calc_pest_phenology()`.

* Added tests confirming that cumulative degree-days equal zero on the biofix
  date.

* Added tests for:

  - missing threshold values for listed pest codes;
  - unknown or unlisted pest codes;
  - invalid trap-data date formats;
  - invalid CIMIS CSV date formats;
  - missing required input columns;
  - lower-threshold flooring of daily degree-days; and
  - upper-threshold capping of daily degree-days.

* Extended test coverage across all weather-input pathways:

  - `cimis_csv`
  - `open_meteo`
  - `daymet`

  Network-dependent tests are conditionally skipped with `skip_if_offline()`
  and `skip_on_cran()`.

## Data

* Updated the bundled `pest_thresholds` reference data from the current
  threshold spreadsheet.

* Corrected selected developmental thresholds and phenological degree-day
  values.

* Added Giant Whitefly (`GWF`), Plum Curculio (`PC`), Tobacco Budworm (`TBW`) and Peach Fruit Fly (`PFF`) to
  `pest_thresholds`.

# TrackTrap 1.0.1

Prior CRAN release.
