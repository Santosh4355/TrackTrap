# TrackTrap 1.1.0


## Biofix and degree-day calculations

* * Standardized biofix as the first positive trap count for every pest.
  This is a package-wide analytical convention and does not establish
  the biological starting point of every published developmental model.

* Degree-day accumulation begins on the day after biofix. Cumulative
  degree-days equal zero on the biofix date. Pre-biofix observations
  receive `NA` for cumulative degree-days from biofix.

* Calculate daily degree-days using the simple-average method.
  Negative daily degree-days are set to zero.

* Added explicit upper-cutoff handling:

  - `upper_cutoff = "horizontal"` caps the daily mean temperature at
    the specified upper threshold before subtracting the lower threshold.
  - `upper_cutoff = "none"` calculates degree-days without upper capping.
  - The default, `upper_cutoff = "explicit"`, applies an available upper
    threshold but requires an explicit choice when it is missing.

* Missing upper thresholds no longer propagate `NA` silently through
  the calculation. Users must select no upper cutoff or supply
  `custom_upper`.

* A finite lower threshold is required. Pests without a database lower
  threshold require `custom_lower`; no threshold is inferred or filled
  automatically.

* Retained the `upper_thresh_defined` output attribute to indicate
  whether an upper threshold was applied.

## Thermal reference markers

* Added `custom_thermal_requirement` for specifying a thermal reference
  value in Fahrenheit degree-days.

* Retained `custom_flight_interval` as a deprecated alias with a warning.

* Database thermal requirements are not automatically treated as
  recurring flight intervals.

* Plot reference lines are labelled as multiples of the thermal
  requirement. Repeated markers require `repeat_markers = TRUE`.

* Thermal markers placed relative to first capture do not independently
  establish flight or generation identities.
  
* Added automatic thermal-reference subtitles and explanatory captions
  to phenology plots when thermal markers are selected. Custom values without a matching
  database reference are labelled as having an unspecified endpoint.

## Weather inputs and reproducibility

* Retained support for Open-Meteo, Daymet, and CIMIS CSV inputs.

* Added support for supplied daily weather data through `weather_data`,
  with `Date`, `tmin`, and `tmax` columns. Temperatures must be in
  Fahrenheit, and `weather_provenance` must be supplied.

* Open-Meteo requests specify an archive model and timezone.
  Returned provenance records the request settings and available
  response metadata.

* Daymet temperatures are converted from Celsius to Fahrenheit.
  Native year/day-of-year dates are retained without shifting or
  filling missing dates.

* Added checks for duplicate weather dates, missing daily observations,
  nonfinite temperatures, and minimum temperatures exceeding maxima.
  Missing weather is not replaced with zero degree-days.

* Attached complete daily weather and calculations to the output as
  the `daily_weather` attribute.

* Attached calculation settings, biofix information, and weather
  provenance as the `calculation_metadata` attribute.
  
* Distinguished `weather_start`, the beginning of the weather input
  period, from `accumulation_start`, the day after biofix.

## CIMIS CSV handling

* CIMIS CSV input requires `Date`, `Max.Air.Temp..F.`, and
  `Min.Air.Temp..F.` columns.

* Station coordinates are not extracted from the CSV. Supplied `lat`
  and `lon` values are retained for plot labelling but do not affect
  calculations using CSV temperatures.

* Coordinates can also be supplied directly to
  `plot_trap_phenology()`.

* Retained the CSV filename and checksum in weather provenance.
  Users can provide station and observation-time details through
  `weather_provenance`.

## Input validation

* Accept dates stored as R `Date` objects or character strings in
  `YYYY-MM-DD`, `M/D/YYYY`, and `MM/DD/YYYY` formats.

* Require nonnegative integer trap counts and one observation per date.

* Require separate analyses for different monitoring years.

* Return an informative error when no positive catch is available
  to establish biofix.
  
* Reject unknown pest codes. Custom analyses use `pest = NULL`
  with user-supplied parameters.

* Reject character dates that do not match the supported formats,
  including dates with trailing arbitrary text.

## Database compatibility

* Support both `thermal_requirement_dd` and the legacy
  `flight_interval_dd` database column.

* Retain available thermal endpoint and source information in
  calculation metadata.

* Database parameter availability alone is not treated as evidence
  that thresholds, thermal requirements, and calculation methods
  constitute a validated biological model.
  
* Retain the complete selected database record in calculation metadata,
  including available sources, endpoints, and verification notes.

# TrackTrap 1.0.1

Prior CRAN release.