# Hypoglycemia-01

The `hypoglycemia_01` function calculates the NEMSQA measure evaluating
how often hypoglycemic patients with altered mental status receive
hypoglycemia treatment.

## Usage

``` r
hypoglycemia_01(
  df = NULL,
  patient_scene_table = NULL,
  response_table = NULL,
  situation_table = NULL,
  vitals_table = NULL,
  medications_table = NULL,
  procedures_table = NULL,
  erecord_01_col,
  incident_date_col = NULL,
  patient_DOB_col = NULL,
  epatient_15_col,
  epatient_16_col,
  eresponse_05_col,
  esituation_11_col,
  esituation_12_col,
  evitals_18_col,
  evitals_23_col,
  evitals_26_col,
  emedications_03_col,
  eprocedures_03_col,
  confidence_interval = FALSE,
  method = c("wilson", "clopper-pearson"),
  conf.level = 0.95,
  correct = TRUE,
  ...
)
```

## Arguments

- df:

  A data frame or tibble containing emergency response records. Default
  is `NULL`.

- patient_scene_table:

  A data.frame or tibble containing at least ePatient and eScene fields
  as a fact table. Default is `NULL`.

- response_table:

  A data.frame or tibble containing at least the eResponse fields needed
  for this measure's calculations. Default is `NULL`.

- situation_table:

  A data.frame or tibble containing at least the eSituation fields
  needed for this measure's calculations. Default is `NULL`.

- vitals_table:

  A data.frame or tibble containing at least the eVitals fields needed
  for this measure's calculations. Default is `NULL`.

- medications_table:

  A data.frame or tibble containing at least the eMedications fields
  needed for this measure's calculations. Default is `NULL`.

- procedures_table:

  A data.frame or tibble containing at least the eProcedures fields
  needed for this measure's calculations. Default is `NULL`.

- erecord_01_col:

  Column representing the unique record identifier.

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
  "Hours", or "Minute").

- eresponse_05_col:

  Column containing response type codes.

- esituation_11_col:

  Column for primary impression fields, containing ICD-10 codes.

- esituation_12_col:

  Column for secondary impression fields, containing ICD-10 codes.

- evitals_18_col:

  Column for blood glucose levels.

- evitals_23_col:

  Column for Glasgow Coma Scale (GCS) scores.

- evitals_26_col:

  Column for AVPU alertness levels.

- emedications_03_col:

  Column for administered medications.

- eprocedures_03_col:

  Column for procedures performed.

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
  esituation_11 = c(rep("E13.64", 3), rep("E16.2", 2)),
  esituation_12 = c(rep("E13.64", 2), rep("E16.2", 3)),
  emedications_03 = c(372326, 376937,
                      377980, 4850,
                      4832),
  evitals_18 = c(60, 59, 58, 57, 56),
  evitals_23 = c(16, 15, 14, 13, 12),
  evitals_26 = c("Alert", "Painful", "Verbal", "Unresponsive", "Alert"),
  eprocedures_03 = rep("710925007", 5)
)

# Run the function
# Return 95% confidence intervals using the Wilson method
hypoglycemia_01(
  df = test_data,
  erecord_01_col = erecord_01,
  epatient_15_col = epatient_15,
  epatient_16_col = epatient_16,
  eresponse_05_col = eresponse_05,
  esituation_11_col = esituation_11,
  esituation_12_col = esituation_12,
  emedications_03_col = emedications_03,
  evitals_18_col = evitals_18,
  evitals_23_col = evitals_23,
  evitals_26_col = evitals_26,
  eprocedures_03_col = eprocedures_03,
  confidence_interval = TRUE
)
#> 
#> ── Hypoglycemia-01 ─────────────────────────────────────────────────────────────
#> 
#> ── Gathering Records for Hypoglycemia-01 ──
#> 
#> Running `hypoglycemia_01_population()`  [Working on 1 of 17 tasks] ●●●─────────…
#> Running `hypoglycemia_01_population()`  [Working on 2 of 17 tasks] ●●●●●───────…
#> Running `hypoglycemia_01_population()`  [Working on 3 of 17 tasks] ●●●●●●──────…
#> Running `hypoglycemia_01_population()`  [Working on 4 of 17 tasks] ●●●●●●●●────…
#> Running `hypoglycemia_01_population()`  [Working on 5 of 17 tasks] ●●●●●●●●●●──…
#> Running `hypoglycemia_01_population()`  [Working on 6 of 17 tasks] ●●●●●●●●●●●●…
#> Running `hypoglycemia_01_population()`  [Working on 7 of 17 tasks] ●●●●●●●●●●●●…
#> Running `hypoglycemia_01_population()`  [Working on 8 of 17 tasks] ●●●●●●●●●●●●…
#> Running `hypoglycemia_01_population()`  [Working on 9 of 17 tasks] ●●●●●●●●●●●●…
#> Running `hypoglycemia_01_population()`  [Working on 10 of 17 tasks] ●●●●●●●●●●●…
#> Running `hypoglycemia_01_population()`  [Working on 11 of 17 tasks] ●●●●●●●●●●●…
#> Running `hypoglycemia_01_population()`  [Working on 12 of 17 tasks] ●●●●●●●●●●●…
#> Running `hypoglycemia_01_population()`  [Working on 13 of 17 tasks] ●●●●●●●●●●●…
#> Running `hypoglycemia_01_population()`  [Working on 14 of 17 tasks] ●●●●●●●●●●●…
#> Running `hypoglycemia_01_population()`  [Working on 15 of 17 tasks] ●●●●●●●●●●●…
#> Running `hypoglycemia_01_population()`  [Working on 16 of 17 tasks] ●●●●●●●●●●●…
#> Running `hypoglycemia_01_population()`  [Working on 17 of 17 tasks] ●●●●●●●●●●●…
#> 
#> 
#> 
#> ── Calculating Hypoglycemia-01 ──
#> 
#> 
#> ✔ Function completed in 0.27s.
#> 
#> Warning: In `prop.test()`: Chi-squared approximation may be incorrect for any n < 10.
#> # A tibble: 3 × 8
#>   measure         pop   numerator denominator  prop prop_label lower_ci upper_ci
#>   <chr>           <chr>     <int>       <int> <dbl> <chr>         <dbl>    <dbl>
#> 1 Hypoglycemia-01 Adul…         2           2     1 100%          0.198        1
#> 2 Hypoglycemia-01 Peds          2           2     1 100%          0.198        1
#> 3 Hypoglycemia-01 All           4           4     1 100%          0.396        1
```
