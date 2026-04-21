# Trauma-01 Calculation

This function processes EMS data to calculate the Trauma-01 performance
measure, which evaluates the percentage of trauma patients assessed for
pain using a numeric scale. The function filters and summarizes the data
based on specified inclusion criteria.

## Usage

``` r
trauma_01(
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
  evitals_23_col,
  evitals_26_col,
  evitals_27_col,
  edisposition_28_col,
  transport_disposition_col,
  confidence_interval = FALSE,
  method = c("wilson", "clopper-pearson"),
  conf.level = 0.95,
  correct = TRUE,
  ...
)
```

## Arguments

- df:

  A data frame or tibble containing EMS records. Default is `NULL`.

- patient_scene_table:

  A data frame or tibble containing only epatient and escene fields as a
  fact table. Default is `NULL`.

- response_table:

  A data frame or tibble containing only the eresponse fields needed for
  this measure's calculations. Default is `NULL`.

- situation_table:

  A data.frame or tibble containing only the esituation fields needed
  for this measure's calculations. Default is `NULL`.

- disposition_table:

  A data.frame or tibble containing only the edisposition fields needed
  for this measure's calculations. Default is `NULL`.

- vitals_table:

  A data.frame or tibble containing only the evitals fields needed for
  this measure's calculations. Default is `NULL`.

- erecord_01_col:

  Column name representing the EMS record ID.

- incident_date_col:

  Column that contains the incident date. This defaults to `NULL` as it
  is optional in case not available due to PII restrictions.

- patient_DOB_col:

  Column that contains the patient's date of birth. This defaults to
  `NULL` as it is optional in case not available due to PII
  restrictions.

- epatient_15_col:

  Column name for the patient's age in numeric format.

- epatient_16_col:

  Column name for the unit of age (e.g., "Years", "Months").

- esituation_02_col:

  Column name indicating if the situation involved an injury.

- eresponse_05_col:

  Column name for the type of EMS response (e.g., 911 call).

- evitals_23_col:

  Column name for the Glasgow Coma Scale (GCS) total score.

- evitals_26_col:

  Column name for AVPU (Alert, Voice, Pain, Unresponsive) status.

- evitals_27_col:

  Column name for the pain scale assessment.

- edisposition_28_col:

  Column name for patient care disposition details.

- transport_disposition_col:

  Column name for transport disposition details.

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
    evitals_23 = rep(15, 5),
    evitals_26 = rep("Alert", 5),
    evitals_27 = c(0, 2, 4, 6, 8),
    edisposition_28 = rep(4228001, 5),
    edisposition_30 = c(4230001, 4230003, 4230001, 4230007, 4230007)
  )

# Run the function
# Return 95% confidence intervals using the Wilson method
  trauma_01(
    df = test_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    esituation_02_col = esituation_02,
    evitals_23_col = evitals_23,
    evitals_26_col = evitals_26,
    evitals_27_col = evitals_27,
    edisposition_28_col = edisposition_28,
    transport_disposition_col = edisposition_30,
    confidence_interval = TRUE
  )
#> 
#> ── Trauma-01 ───────────────────────────────────────────────────────────────────
#> 
#> ── Gathering Records for Trauma-01 ──
#> 
#> Running `trauma_01_population()`  [Working on 1 of 14 tasks] ●●●───────────────…
#> Running `trauma_01_population()`  [Working on 2 of 14 tasks] ●●●●●─────────────…
#> Running `trauma_01_population()`  [Working on 3 of 14 tasks] ●●●●●●●───────────…
#> Running `trauma_01_population()`  [Working on 4 of 14 tasks] ●●●●●●●●●●────────…
#> Running `trauma_01_population()`  [Working on 5 of 14 tasks] ●●●●●●●●●●●●──────…
#> Running `trauma_01_population()`  [Working on 6 of 14 tasks] ●●●●●●●●●●●●●●────…
#> Running `trauma_01_population()`  [Working on 7 of 14 tasks] ●●●●●●●●●●●●●●●●──…
#> Running `trauma_01_population()`  [Working on 8 of 14 tasks] ●●●●●●●●●●●●●●●●●●…
#> Running `trauma_01_population()`  [Working on 9 of 14 tasks] ●●●●●●●●●●●●●●●●●●…
#> Running `trauma_01_population()`  [Working on 10 of 14 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_01_population()`  [Working on 11 of 14 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_01_population()`  [Working on 12 of 14 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_01_population()`  [Working on 13 of 14 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_01_population()`  [Working on 14 of 14 tasks] ●●●●●●●●●●●●●●●●●…
#> 
#> 
#> 
#> ── Calculating Trauma-01 ──
#> 
#> 
#> ✔ Function completed in 0.23s.
#> 
#> Warning: In `prop.test()`: Chi-squared approximation may be incorrect for any n < 10.
#> # A tibble: 3 × 8
#>   measure   pop    numerator denominator  prop prop_label lower_ci upper_ci
#>   <chr>     <chr>      <int>       <int> <dbl> <chr>         <dbl>    <dbl>
#> 1 Trauma-01 Adults         3           3     1 100%         0.310         1
#> 2 Trauma-01 Peds           1           1     1 100%         0.0546        1
#> 3 Trauma-01 All            5           5     1 100%         0.463         1
```
