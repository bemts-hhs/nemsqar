# Stroke-01 Calculation

The `stroke_01` function processes EMS dataset to identify potential
stroke cases based on specific criteria and calculates the stroke scale
measures. It filters the data for 911 response calls, identifies
stroke-related impressions and scales, and aggregates results by unique
patient encounters.

## Usage

``` r
stroke_01(
  df = NULL,
  patient_scene_table = NULL,
  response_table = NULL,
  situation_table = NULL,
  vitals_table = NULL,
  erecord_01_col,
  eresponse_05_col,
  esituation_11_col,
  esituation_12_col,
  evitals_23_col,
  evitals_26_col,
  evitals_29_col,
  evitals_30_col,
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

- eresponse_05_col:

  Column that contains eResponse.05 or the response type.

- esituation_11_col:

  Column that contains eSituation.11 provider primary impression data.

- esituation_12_col:

  Column that contains all eSituation.12 values as (possible a single
  comma-separated list), provider secondary impression data.

- evitals_23_col:

  Column for Glasgow Coma Scale (GCS) scores.

- evitals_26_col:

  Column for AVPU alertness levels.

- evitals_29_col:

  The column containing the stroke scale score achieved during
  assessment.

- evitals_30_col:

  The column containing stroke scale type descriptors (e.g., FAST, NIH,
  etc.).

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
    esituation_11 = c(rep("I60", 3), rep("I61", 2)),
    esituation_12 = c(rep("I63", 2), rep("I64", 3)),
    evitals_23 = c(16, 15, 14, 13, 12),
    evitals_26 = c("Alert", "Painful", "Verbal", "Unresponsive", "Alert"),
    evitals_29 = rep("positive", 5),
    evitals_30 = rep("a pain scale", 5)
  )

# Run the function
# Return 95% confidence intervals using the Wilson method
  stroke_01(
    df = test_data,
    erecord_01_col = erecord_01,
    incident_date_col = NULL,
    patient_DOB_col = NULL,
    eresponse_05_col = eresponse_05,
    esituation_11_col = esituation_11,
    esituation_12_col = esituation_12,
    evitals_23_col = evitals_23,
    evitals_26_col = evitals_26,
    evitals_29_col = evitals_29,
    evitals_30_col = evitals_30,
    confidence_interval = TRUE
  )
#> 
#> ── Stroke-01 ───────────────────────────────────────────────────────────────────
#> 
#> ── Gathering Records for Stroke-01 ──
#> 
#> Running `stroke_01_population()`  [Working on 1 of 11 tasks] ●●●●──────────────…
#> Running `stroke_01_population()`  [Working on 2 of 11 tasks] ●●●●●●────────────…
#> Running `stroke_01_population()`  [Working on 3 of 11 tasks] ●●●●●●●●●─────────…
#> Running `stroke_01_population()`  [Working on 4 of 11 tasks] ●●●●●●●●●●●●──────…
#> Running `stroke_01_population()`  [Working on 5 of 11 tasks] ●●●●●●●●●●●●●●●───…
#> Running `stroke_01_population()`  [Working on 6 of 11 tasks] ●●●●●●●●●●●●●●●●●─…
#> Running `stroke_01_population()`  [Working on 7 of 11 tasks] ●●●●●●●●●●●●●●●●●●…
#> Running `stroke_01_population()`  [Working on 8 of 11 tasks] ●●●●●●●●●●●●●●●●●●…
#> Running `stroke_01_population()`  [Working on 9 of 11 tasks] ●●●●●●●●●●●●●●●●●●…
#> Running `stroke_01_population()`  [Working on 10 of 11 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `stroke_01_population()`  [Working on 11 of 11 tasks] ●●●●●●●●●●●●●●●●●…
#> 
#> 
#> 
#> ── Calculating Stroke-01 ──
#> 
#> 
#> ✔ Function completed in 0.16s.
#> 
#> Warning: In `prop.test()`: Chi-squared approximation may be incorrect for any n < 10.
#> # A tibble: 1 × 8
#>   measure   pop   numerator denominator  prop prop_label lower_ci upper_ci
#>   <chr>     <chr>     <int>       <int> <dbl> <chr>         <dbl>    <dbl>
#> 1 Stroke-01 All           5           5     1 100%          0.463        1
```
