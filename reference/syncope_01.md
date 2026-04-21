# Syncope-01 Calculation

The `syncope_01` function processes EMS dataset to identify potential
syncope (fainting) cases based on specific criteria and calculates
related ECG measures. This function dplyr::filters data for 911 response
calls, assesses primary and associated symptoms for syncope, determines
age-based populations (adult and pediatric), and aggregates results by
unique patient encounters.

## Usage

``` r
syncope_01(
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
  esituation_09_col,
  esituation_10_col,
  esituation_11_col,
  esituation_12_col,
  evitals_04_col,
  confidence_interval = FALSE,
  method = c("wilson", "clopper-pearson"),
  conf.level = 0.95,
  correct = TRUE,
  ...
)
```

## Arguments

- df:

  Main data frame containing EMS records.

- patient_scene_table:

  A data frame or tibble containing only epatient and escene fields as a
  fact table. Default is `NULL`.

- response_table:

  A data frame or tibble containing only the eresponse fields needed for
  this measure's calculations. Default is `NULL`.

- situation_table:

  A data.frame or tibble containing only the esituation fields needed
  for this measure's calculations. Default is `NULL`.

- vitals_table:

  A data.frame or tibble containing only the evitals fields needed for
  this measure's calculations. Default is `NULL`.

- erecord_01_col:

  The column containing unique record identifiers for each encounter.

- incident_date_col:

  Column that contains the incident date. This defaults to `NULL` as it
  is optional in case not available due to PII restrictions.

- patient_DOB_col:

  Column that contains the patient's date of birth. This defaults to
  `NULL` as it is optional in case not available due to PII
  restrictions.

- epatient_15_col:

  Column representing the patient age (numeric).

- epatient_16_col:

  Column for the patient age units (e.g., "Years", "Months").

- eresponse_05_col:

  Column containing response type codes, specifically 911 codes.

- esituation_09_col:

  Column with primary symptoms associated with the patient encounter.

- esituation_10_col:

  Column with other associated symptoms.

- esituation_11_col:

  Column for primary impression code.

- esituation_12_col:

  Column for secondary impression codes.

- evitals_04_col:

  Column with ECG information if available.

- confidence_interval:

  **\[experimental\]** Logical. If `TRUE`, the function calculates a
  confidence interval for the proportion estimate.

- method:

  **\[experimental\]**Character. Specifies the method used to calculate
  confidence intervals. Options are `"wilson"` (Wilson score interval)
  and `"clopper-pearson"` (exact binomial interval). Partial matching is
  supported, so `"w"` and `"c"` can be used as shorthand.

- conf.level:

  **\[experimental\]**Numeric. The confidence level for the interval,
  expressed as a proportion (e.g., 0.95 for a 95% confidence interval).
  Defaults to 0.95.

- correct:

  **\[experimental\]**Logical. If `TRUE`, applies a continuity
  correction to the Wilson score interval when `method = "wilson"`.
  Defaults to `TRUE`.

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
    esituation_09 = c(rep("R55", 3), rep("R40.4", 2)),
    esituation_10 = c(rep("R40.4", 2), rep("R55", 3)),
    esituation_11 = c(rep("R55", 3), rep("R40.4", 2)),
    esituation_12 = c(rep("R40.4", 2), rep("R55", 3)),
    evitals_04 = rep("15 Lead", 5)
  )

# Run the function
# Return 95% confidence intervals using the Wilson method
  syncope_01(
    df = test_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    esituation_09_col = esituation_09,
    esituation_10_col = esituation_10,
    esituation_11_col = esituation_11,
    esituation_12_col = esituation_12,
    evitals_04_col = evitals_04,
    confidence_interval = TRUE
  )
#> 
#> ── Syncope-01 ──────────────────────────────────────────────────────────────────
#> 
#> ── Gathering Records for Syncope-01 ──
#> 
#> Running `syncope_01_population()`  [Working on 1 of 10 tasks] ●●●●─────────────…
#> Running `syncope_01_population()`  [Working on 2 of 10 tasks] ●●●●●●●──────────…
#> Running `syncope_01_population()`  [Working on 3 of 10 tasks] ●●●●●●●●●●───────…
#> Running `syncope_01_population()`  [Working on 4 of 10 tasks] ●●●●●●●●●●●●●────…
#> Running `syncope_01_population()`  [Working on 5 of 10 tasks] ●●●●●●●●●●●●●●●●─…
#> Running `syncope_01_population()`  [Working on 6 of 10 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `syncope_01_population()`  [Working on 7 of 10 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `syncope_01_population()`  [Working on 8 of 10 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `syncope_01_population()`  [Working on 9 of 10 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `syncope_01_population()`  [Working on 10 of 10 tasks] ●●●●●●●●●●●●●●●●…
#> 
#> 
#> 
#> ── Calculating Syncope-01 ──
#> 
#> 
#> ✔ Function completed in 0.17s.
#> 
#> Warning: In `prop.test()`: Chi-squared approximation may be incorrect for any n < 10.
#> # A tibble: 2 × 8
#>   measure    pop    numerator denominator  prop prop_label lower_ci upper_ci
#>   <chr>      <chr>      <int>       <int> <dbl> <chr>         <dbl>    <dbl>
#> 1 Syncope-01 Adults         3           3     1 100%          0.310        1
#> 2 Syncope-01 Peds           2           2     1 100%          0.198        1
```
