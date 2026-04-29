# TBI-01 Calculation

This function screens for potential traumatic brain injury (TBI) cases
based on specific criteria in a patient dataset. It produces a subset of
the data with calculated variables for TBI identification.

## Usage

``` r
tbi_01(
  df = NULL,
  patient_scene_table = NULL,
  response_table = NULL,
  situation_table = NULL,
  disposition_table = NULL,
  vitals_table = NULL,
  erecord_01_col,
  incident_date_col = NULL,
  patient_DOB_col = NULL,
  epatient_15_col,
  epatient_16_col,
  eresponse_05_col,
  esituation_11_col,
  esituation_12_col,
  transport_disposition_col,
  evitals_06_col,
  evitals_12_col,
  evitals_16_col,
  evitals_23_col,
  evitals_26_col,
  confidence_interval = FALSE,
  method = c("wilson", "clopper-pearson"),
  conf.level = 0.95,
  correct = TRUE,
  ...
)
```

## Arguments

- df:

  A dataframe or tibble contianing EMS data where each row represents an
  observation and columns represent features.

- patient_scene_table:

  A data.frame or tibble containing at least ePatient, and eScene as a
  fact table.

- response_table:

  A data.frame or tibble containing at least the eResponse fields needed
  for this measure's calculations.

- situation_table:

  A data.frame or tibble containing at least the eSituation fields
  needed for this measure's calculations. Default is `NULL`.

- disposition_table:

  A data.frame or tibble containing only the edisposition fields needed
  for this measure's calculations.

- vitals_table:

  A dataframe or tibble containing at least the eVitals fields needed.

- erecord_01_col:

  The column representing the EMS record unique identifier.

- incident_date_col:

  Column that contains the incident date. This defaults to `NULL` as it
  is optional in case not available due to PII restrictions.

- patient_DOB_col:

  Column that contains the patient's date of birth. This defaults to
  `NULL` as it is optional in case not available due to PII
  restrictions.

- epatient_15_col:

  Column representing the patient's numeric age agnostic of unit.

- epatient_16_col:

  Column representing the patient's age unit ("Years", "Months", "Days",
  "Hours", or "Minutes").

- eresponse_05_col:

  Column that contains eResponse.05 or the response type.

- esituation_11_col:

  Column that contains eSituation.11 provider primary impression data.

- esituation_12_col:

  Column that contains all eSituation.12 values as (possible a single
  comma-separated list), provider secondary impression data.

- transport_disposition_col:

  One or more unquoted column names (such as edisposition.12,
  edisposition.30) containing transport disposition for an EMS event
  identifying whether a transport occurred and by which unit.

- evitals_06_col:

  Numeric column containing systolic blood pressure values.

- evitals_12_col:

  Numeric column containing pulse oximetry values.

- evitals_16_col:

  Column with numeric value of the patient's exhaled end tidal carbon
  dioxide (ETCO2) level measured as a unit of pressure in millimeters of
  mercury (mmHg), percentage or, kilopascal (kPa).

- evitals_23_col:

  Column for Glasgow Coma Scale (GCS) scores.

- evitals_26_col:

  Column for AVPU alertness levels.

- confidence_interval:

  Logical. If `TRUE`, the function calculates a confidence interval for
  the proportion estimate.

- method:

  Character. Specifies the method used to calculate confidence
  intervals. Options are `"wilson"` (Wilson score interval) and
  `"clopper-pearson"` (exact binomial interval). Partial matching is
  supported, so `"w"` and `"c"` can be used as shorthand.

- conf.level:

  Numeric. The confidence level for the interval, expressed as a
  proportion (e.g., 0.95 for a 95% confidence interval). Defaults to
  0.95.

- correct:

  Logical. If `TRUE`, applies a continuity correction to the Wilson
  score interval when `method = "wilson"`. Defaults to `TRUE`.

- ...:

  optional additional arguments to pass onto
  [`dplyr::summarize`](https://dplyr.tidyverse.org/reference/summarise.html).

## Value

A data.frame summarizing results for two population groups (All, Adults
and Peds) with the following columns:

- `pop`: Population type (All, Adults, and Peds).

- `numerator`: Count of incidents meeting the measure.

- `denominator`: Total count of included incidents.

- `prop`: Proportion of incidents meeting the measure.

- `prop_label`: Proportion formatted as a percentage with a specified
  number of decimal places.

- `lower_ci`: Lower bound of the confidence interval for `prop` (if
  `confidence_interval = TRUE`).

- `upper_ci`: Upper bound of the confidence interval for `prop` (if
  `confidence_interval = TRUE`).

## Author

Nicolas Foss, Ed.D., MS

## Examples

``` r
# Synthetic test data
  test_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    esituation_11 = c(rep("S02", 3), rep("S06", 2)),
    esituation_12 = c(rep("S09.90", 2), rep("S06.0X9", 3)),
    evitals_06 = c(85, 80, 100, 90, 82),
    evitals_12 = c(95, 96, 97, 98, 99),
    evitals_16 = c(35, 36, 37, 38, 39),
    evitals_23 = rep(8, 5),
    evitals_26 = c("Verbal", "Painful", "Unresponsive", "Verbal", "Painful"),
    edisposition_30 = c(4230001, 4230003, 4230001, 4230007, 4230007)
  )

# Run the function
# Return 95% confidence intervals using the Wilson method
  tbi_01(
    df = test_data,
    erecord_01_col = erecord_01,
    incident_date_col = NULL,
    patient_DOB_col = NULL,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    esituation_11_col = esituation_11,
    esituation_12_col = esituation_12,
    evitals_06_col = evitals_06,
    evitals_12_col = evitals_12,
    evitals_16_col = evitals_16,
    evitals_23_col = evitals_23,
    evitals_26_col = evitals_26,
    transport_disposition_col = edisposition_30,
    confidence_interval = TRUE
  )
#> 
#> ── TBI-01 ──────────────────────────────────────────────────────────────────────
#> 
#> ── Gathering Records for TBI-01 ──
#> 
#> Running `tbi_01_population()`  [Working on 1 of 15 tasks] ●●●──────────────────…
#> Running `tbi_01_population()`  [Working on 2 of 15 tasks] ●●●●●────────────────…
#> Running `tbi_01_population()`  [Working on 5 of 15 tasks] ●●●●●●●●●●●──────────…
#> Running `tbi_01_population()`  [Working on 6 of 15 tasks] ●●●●●●●●●●●●●────────…
#> Running `tbi_01_population()`  [Working on 7 of 15 tasks] ●●●●●●●●●●●●●●●──────…
#> Running `tbi_01_population()`  [Working on 8 of 15 tasks] ●●●●●●●●●●●●●●●●●────…
#> Running `tbi_01_population()`  [Working on 9 of 15 tasks] ●●●●●●●●●●●●●●●●●●●──…
#> Running `tbi_01_population()`  [Working on 10 of 15 tasks] ●●●●●●●●●●●●●●●●●●●●…
#> Running `tbi_01_population()`  [Working on 12 of 15 tasks] ●●●●●●●●●●●●●●●●●●●●…
#> Running `tbi_01_population()`  [Working on 13 of 15 tasks] ●●●●●●●●●●●●●●●●●●●●…
#> Running `tbi_01_population()`  [Working on 14 of 15 tasks] ●●●●●●●●●●●●●●●●●●●●…
#> Running `tbi_01_population()`  [Working on 15 of 15 tasks] ●●●●●●●●●●●●●●●●●●●●…
#> 
#> 
#> 
#> ── Calculating TBI-01 ──
#> 
#> 
#> ✔ Function completed in 0.19s.
#> 
#> Warning: In `prop.test()`: Chi-squared approximation may be incorrect for any n < 10.
#> # A tibble: 2 × 8
#>   measure pop    numerator denominator  prop prop_label lower_ci upper_ci
#>   <chr>   <chr>      <int>       <int> <dbl> <chr>         <dbl>    <dbl>
#> 1 TBI-01  Adults         3           3     1 100%          0.310        1
#> 2 TBI-01  Peds           2           2     1 100%          0.198        1
```
