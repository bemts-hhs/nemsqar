# Pediatrics-03B Calculation

The function calculates a pediatric metric focused on EMS responses,
specifically targeting responses that involve patients under 18 years of
age, where certain weight-based medications were administered. This
function filters EMS data to identify relevant 911 responses and further
narrows down the dataset to cases involving children, calculating the
proportion of cases with documented weight among those where
weight-based medications were administered.

## Usage

``` r
pediatrics_03b(
  df = NULL,
  patient_scene_table = NULL,
  response_table = NULL,
  exam_table = NULL,
  medications_table = NULL,
  erecord_01_col,
  incident_date_col = NULL,
  patient_DOB_col = NULL,
  epatient_15_col,
  epatient_16_col,
  eresponse_05_col,
  eexam_01_col,
  eexam_02_col,
  emedications_03_col,
  emedications_04_col,
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

  A data.frame or tibble containing only ePatient and eScene fields as a
  fact table. Default is `NULL`.

- response_table:

  A data.frame or tibble containing only the eResponse fields needed for
  this measure's calculations. Default is `NULL`.

- exam_table:

  A data.frame or tibble containing only the eExam fields needed for
  this measure's calculations. Default is `NULL`.

- medications_table:

  A data.frame or tibble containing only the eMedications fields needed
  for this measure's calculations. Default is `NULL`.

- erecord_01_col:

  Column for unique EMS record identifiers.

- incident_date_col:

  Column that contains the incident date. This defaults to `NULL` as it
  is optional in case not available due to PII restrictions.

- patient_DOB_col:

  Column that contains the patient's date of birth. This defaults to
  `NULL` as it is optional in case not available due to PII
  restrictions.

- epatient_15_col:

  Column giving the calculated age value.

- epatient_16_col:

  Column giving the provided age unit value.

- eresponse_05_col:

  Column containing the EMS response codes.

- eexam_01_col:

  Column containing documented weight information.

- eexam_02_col:

  Another column for weight documentation, if applicable.

- emedications_03_col:

  Column indicating medication administration.

- emedications_04_col:

  Column listing medications administered.

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

A data.frame summarizing results for two population groups (Peds) with
the following columns:

- `pop`: Population type (Peds).

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
  incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01",
  "2025-06-01", "2025-12-15")),
  patient_dob = as.Date(c("2021-01-01", "2020-01-01", "2022-02-01",
  "2023-06-01", "2019-12-15")),
  epatient_15 = c(4, 5, 3, 2, 6),  # Ages
  epatient_16 = c("Years", "Years", "Years", "Years", "Years"),
  eresponse_05 = rep(2205001, 5),
  emedications_03 = rep("stuff", 5),
  emedications_04 = c("Inhalation", "pill", "liquid", "pill", "liquid"),
  eexam_01 = c(60, 59, 58, 57, 56),
  eexam_02 = c("Red", "Purple", "Grey", "Yellow", "Orange")
)

# Run the function
# Return 95% confidence intervals using the Wilson method
pediatrics_03b(
  df = test_data,
  erecord_01_col = erecord_01,
  incident_date_col = incident_date,
  patient_DOB_col = patient_dob,
  epatient_15_col = epatient_15,
  epatient_16_col = epatient_16,
  eresponse_05_col = eresponse_05,
  emedications_03_col = emedications_03,
  emedications_04_col = emedications_04,
  eexam_01_col = eexam_01,
  eexam_02_col = eexam_02,
  confidence_interval = TRUE
)
#> 
#> ── Pediatrics-03b ──────────────────────────────────────────────────────────────
#> 
#> ── Gathering Records for Pediatrics-03b ──
#> 
#> Running `pediatrics_03b_population()`  [Working on 1 of 9 tasks] ●●●●──────────…
#> Running `pediatrics_03b_population()`  [Working on 2 of 9 tasks] ●●●●●●●●──────…
#> Running `pediatrics_03b_population()`  [Working on 3 of 9 tasks] ●●●●●●●●●●●───…
#> Running `pediatrics_03b_population()`  [Working on 4 of 9 tasks] ●●●●●●●●●●●●●●…
#> Running `pediatrics_03b_population()`  [Working on 5 of 9 tasks] ●●●●●●●●●●●●●●…
#> Running `pediatrics_03b_population()`  [Working on 6 of 9 tasks] ●●●●●●●●●●●●●●…
#> Running `pediatrics_03b_population()`  [Working on 7 of 9 tasks] ●●●●●●●●●●●●●●…
#> Running `pediatrics_03b_population()`  [Working on 8 of 9 tasks] ●●●●●●●●●●●●●●…
#> Running `pediatrics_03b_population()`  [Working on 9 of 9 tasks] ●●●●●●●●●●●●●●…
#> 
#> 
#> 
#> ── Calculating Pediatrics-03b ──
#> 
#> 
#> ✔ Function completed in 0.16s.
#> 
#> Warning: In `prop.test()`: Chi-squared approximation may be incorrect for any n < 10.
#> # A tibble: 1 × 8
#>   measure        pop   numerator denominator  prop prop_label lower_ci upper_ci
#>   <chr>          <chr>     <int>       <int> <dbl> <chr>         <dbl>    <dbl>
#> 1 Pediatrics-03b Peds          4           4     1 100%          0.396        1
```
