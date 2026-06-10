# Respiratory-01 Calculation

The `respiratory_01` function filters and analyzes data related to
emergency 911 respiratory distress incidents, providing summary
statistics for adult and pediatric populations. This function uses
specific data columns for 911 response codes, primary and secondary
impressions, and vital signs to calculate the proportion of cases with
complete vital signs recorded, stratified by age.

## Usage

``` r
respiratory_01(
  df = NULL,
  patient_scene_table = NULL,
  response_table = NULL,
  situation_table = NULL,
  vitals_table = NULL,
  erecord_01_col,
  incident_date_col = NULL,
  patient_DOB_col = NULL,
  epatient_15_col,
  epatient_16_col,
  eresponse_05_col,
  esituation_11_col,
  esituation_12_col,
  evitals_12_col,
  evitals_14_col,
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

- evitals_12_col:

  Numeric column containing pulse oximetry values.

- evitals_14_col:

  Column containing data on patient's respiratory rate expressed as a
  number per minute.

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
  esituation_11 = c(rep("J80", 3), rep("I50.9", 2)),
  esituation_12 = c(rep("J80", 2), rep("I50.9", 3)),
  evitals_12 = c(60, 59, 58, 57, 56),
  evitals_14 = c(16, 15, 14, 13, 12)
)

# Run the function
# Return 95% confidence intervals using the Wilson method
respiratory_01(
  df = test_data,
  erecord_01_col = erecord_01,
  incident_date_col = NULL,
  patient_DOB_col = NULL,
  epatient_15_col = epatient_15,
  epatient_16_col = epatient_16,
  eresponse_05_col = eresponse_05,
  esituation_11_col = esituation_11,
  esituation_12_col = esituation_12,
  evitals_12_col = evitals_12,
  evitals_14_col = evitals_14,
  confidence_interval = TRUE
)
#> 
#> ── Respiratory-01 ──────────────────────────────────────────────────────────────
#> 
#> ── Gathering Records for Respiratory-01 ──
#> 
#> Running `respiratory_01_population()`  [Working on 1 of 13 tasks] ●●●──────────…
#> Running `respiratory_01_population()`  [Working on 2 of 13 tasks] ●●●●●●───────…
#> Running `respiratory_01_population()`  [Working on 3 of 13 tasks] ●●●●●●●●─────…
#> Running `respiratory_01_population()`  [Working on 4 of 13 tasks] ●●●●●●●●●●───…
#> Running `respiratory_01_population()`  [Working on 5 of 13 tasks] ●●●●●●●●●●●●●…
#> Running `respiratory_01_population()`  [Working on 6 of 13 tasks] ●●●●●●●●●●●●●…
#> Running `respiratory_01_population()`  [Working on 7 of 13 tasks] ●●●●●●●●●●●●●…
#> Running `respiratory_01_population()`  [Working on 8 of 13 tasks] ●●●●●●●●●●●●●…
#> Running `respiratory_01_population()`  [Working on 9 of 13 tasks] ●●●●●●●●●●●●●…
#> Running `respiratory_01_population()`  [Working on 11 of 13 tasks] ●●●●●●●●●●●●…
#> Running `respiratory_01_population()`  [Working on 12 of 13 tasks] ●●●●●●●●●●●●…
#> Running `respiratory_01_population()`  [Working on 13 of 13 tasks] ●●●●●●●●●●●●…
#> 
#> 
#> 
#> ── Calculating Respiratory-01 ──
#> 
#> 
#> ✔ Function completed in 0.18s.
#> 
#> Warning: In `prop.test()`: Chi-squared approximation may be incorrect for any n < 10.
#> # A tibble: 3 × 8
#>   measure        pop    numerator denominator  prop prop_label lower_ci upper_ci
#>   <chr>          <chr>      <int>       <int> <dbl> <chr>         <dbl>    <dbl>
#> 1 Respiratory-01 Adults         3           3     1 100%          0.310        1
#> 2 Respiratory-01 Peds           2           2     1 100%          0.198        1
#> 3 Respiratory-01 All            5           5     1 100%          0.463        1
```
