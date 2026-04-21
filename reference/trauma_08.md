# Trauma-08 Calculation

This function calculates the Trauma-08 measure, which evaluates the

## Usage

``` r
trauma_08(
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
  esituation_02_col,
  eresponse_05_col,
  transport_disposition_col,
  evitals_06_col,
  evitals_14_col,
  evitals_23_col,
  confidence_interval = FALSE,
  method = c("wilson", "clopper-pearson"),
  conf.level = 0.95,
  correct = TRUE,
  ...
)
```

## Arguments

- df:

  A data frame or tibble containing EMS data with all relevant columns.
  Default is `NULL`.

- patient_scene_table:

  A data frame or tibble containing only epatient and escene fields as a
  fact table. Default is `NULL`.

- response_table:

  A data frame or tibble containing only the eresponse fields needed for
  this measure's calculations. Default is `NULL`.

- situation_table:

  A data frame or tibble containing only the esituation fields needed
  for this measure's calculations. Default is `NULL`.

- disposition_table:

  A data frame or tibble containing only the edisposition fields needed
  for this measure's calculations. Default is `NULL`.

- vitals_table:

  A data frame or tibble containing only the evitals fields needed for
  this measure's calculations. Default is `NULL`.

- erecord_01_col:

  A column specifying unique patient records.

- incident_date_col:

  Column that contains the incident date. This defaults to `NULL` as it
  is optional in case not available due to PII restrictions.

- patient_DOB_col:

  Column that contains the patient's date of birth. This defaults to
  `NULL` as it is optional in case not available due to PII
  restrictions.

- epatient_15_col:

  A column indicating the patient’s age in numeric form.

- epatient_16_col:

  A column specifying the unit of patient age (e.g., "Years", "Days").

- esituation_02_col:

  A column containing information about the nature of the patient’s
  condition (e.g., injury type).

- eresponse_05_col:

  A column specifying the type of response (e.g., 911 codes).

- transport_disposition_col:

  A column specifying transport disposition for the patient.

- evitals_06_col:

  A column containing systolic blood pressure (SBP) data from initial
  vital signs.

- evitals_14_col:

  A column containing respiratory rate data from initial vital signs.

- evitals_23_col:

  A column containing total Glasgow Coma Scale (GCS) scores from initial
  vital signs.

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
    esituation_02 = rep("Yes", 5),
    evitals_06 = c(100, 90, 80, 70, 85),
    evitals_14 = c(30, 9, 8, 7, 31),
    evitals_23 = c(6, 7, 8, 8, 7),
    edisposition_30 = c(4230001, 4230003, 4230001, 4230007, 4230007)
  )

  # Run function with the first and last pain score columns
  # Return 95% confidence intervals using the Wilson method
  trauma_08(
    df = test_data,
    erecord_01_col = erecord_01,
    incident_date_col = NULL,
    patient_DOB_col = NULL,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    esituation_02_col = esituation_02,
    evitals_06_col = evitals_06,
    evitals_14_col = evitals_14,
    evitals_23_col = evitals_23,
    transport_disposition_col = edisposition_30,
    confidence_interval = TRUE
  )
#> 
#> ── Trauma-08 ───────────────────────────────────────────────────────────────────
#> 
#> ── Gathering Records for Trauma-08 ──
#> 
#> Running `trauma_08_population()`  [Working on 1 of 12 tasks] ●●●───────────────…
#> Running `trauma_08_population()`  [Working on 2 of 12 tasks] ●●●●●●────────────…
#> Running `trauma_08_population()`  [Working on 3 of 12 tasks] ●●●●●●●●●─────────…
#> Running `trauma_08_population()`  [Working on 4 of 12 tasks] ●●●●●●●●●●●───────…
#> Running `trauma_08_population()`  [Working on 7 of 12 tasks] ●●●●●●●●●●●●●●●●●●…
#> Running `trauma_08_population()`  [Working on 8 of 12 tasks] ●●●●●●●●●●●●●●●●●●…
#> Running `trauma_08_population()`  [Working on 9 of 12 tasks] ●●●●●●●●●●●●●●●●●●…
#> Running `trauma_08_population()`  [Working on 10 of 12 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_08_population()`  [Working on 11 of 12 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_08_population()`  [Working on 12 of 12 tasks] ●●●●●●●●●●●●●●●●●…
#> 
#> 
#> 
#> ── Calculating Trauma-08 ──
#> 
#> 
#> ✔ Function completed in 0.17s.
#> 
#> Warning: In `prop.test()`: Chi-squared approximation may be incorrect for any n < 10.
#> # A tibble: 2 × 8
#>   measure   pop    numerator denominator  prop prop_label lower_ci upper_ci
#>   <chr>     <chr>      <int>       <int> <dbl> <chr>         <dbl>    <dbl>
#> 1 Trauma-08 Adults         3           3     1 100%          0.310        1
#> 2 Trauma-08 Peds           2           2     1 100%          0.198        1
```
