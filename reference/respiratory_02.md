# Respiratory-02 Calculation

The `respiratory_02` function calculates metrics for pediatric and adult
respiratory populations based on pre-defined criteria, such as low
oxygen saturation and specific medication or procedure codes. It returns
a summary table of the overall, pediatric, and adult populations,
showing counts and proportions.

## Usage

``` r
respiratory_02(
  df = NULL,
  patient_scene_table = NULL,
  response_table = NULL,
  vitals_table = NULL,
  medications_table = NULL,
  procedures_table = NULL,
  erecord_01_col,
  incident_date_col = NULL,
  patient_DOB_col = NULL,
  epatient_15_col,
  epatient_16_col,
  eresponse_05_col,
  evitals_12_col,
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

  A dataframe or tibble contianing EMS data where each row represents an
  observation and columns represent features.

- patient_scene_table:

  A data.frame or tibble containing at least ePatient, and eScene as a
  fact table.

- response_table:

  A data.frame or tibble containing at least the eResponse fields needed
  for this measure's calculations.

- vitals_table:

  A dataframe or tibble containing at least the eVitals fields needed.

- medications_table:

  A data.frame or tibble containing at least the eMedications fields
  needed for this measure's calculations. Default is `NULL`.

- procedures_table:

  A dataframe or tibble containing at least the eProcedures fields
  needed.

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

- evitals_12_col:

  Numeric column containing pulse oximetry values.

- emedications_03_col:

  Column that contains all medication administered to the patient
  (eMedications.03) values as a single comma-separated list per distinct
  eRecord.01 ID.

- eprocedures_03_col:

  Column containing procedure codes with or without procedure names.

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
    emedications_03 = c("Oxygen", "Oxygen", "Oxygen", "Oxygen", "Oxygen"),
    evitals_12 = c(60, 59, 58, 57, 56),
    eprocedures_03 = rep("applicable thing", 5)
  )

# Run the function
# Return 95% confidence intervals using the Wilson method
  respiratory_02(
    df = test_data,
    erecord_01_col = erecord_01,
    incident_date_col = NULL,
    patient_DOB_col = NULL,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    emedications_03_col = emedications_03,
    evitals_12_col = evitals_12,
    eprocedures_03_col = eprocedures_03,
    confidence_interval = TRUE
  )
#> 
#> ── Respiratory-02 ──────────────────────────────────────────────────────────────
#> 
#> ── Gathering Records for Respiratory-02 ──
#> 
#> Running `respiratory_02_population()`  [Completed 1 of 11 tasks] ●●●●──────────…
#> Running `respiratory_02_population()`  [Completed 2 of 11 tasks] ●●●●●●────────…
#> Running `respiratory_02_population()`  [Completed 3 of 11 tasks] ●●●●●●●●●─────…
#> Running `respiratory_02_population()`  [Completed 4 of 11 tasks] ●●●●●●●●●●●●──…
#> Running `respiratory_02_population()`  [Completed 5 of 11 tasks] ●●●●●●●●●●●●●●…
#> Running `respiratory_02_population()`  [Completed 6 of 11 tasks] ●●●●●●●●●●●●●●…
#> Running `respiratory_02_population()`  [Completed 7 of 11 tasks] ●●●●●●●●●●●●●●…
#> Running `respiratory_02_population()`  [Completed 8 of 11 tasks] ●●●●●●●●●●●●●●…
#> Running `respiratory_02_population()`  [Completed 9 of 11 tasks] ●●●●●●●●●●●●●●…
#> Running `respiratory_02_population()`  [Completed 10 of 11 tasks] ●●●●●●●●●●●●●…
#> Running `respiratory_02_population()`  [Completed 11 of 11 tasks] ●●●●●●●●●●●●●…
#> 
#> 
#> 
#> ── Calculating Respiratory-02 ──
#> 
#> 
#> ✔ Function completed in 0.17s.
#> 
#> Warning: In `prop.test()`: Chi-squared approximation may be incorrect for any n < 10.
#> # A tibble: 3 × 8
#>   measure        pop    numerator denominator  prop prop_label lower_ci upper_ci
#>   <chr>          <chr>      <int>       <int> <dbl> <chr>         <dbl>    <dbl>
#> 1 Respiratory-02 Adults         3           3     1 100%          0.310        1
#> 2 Respiratory-02 Peds           2           2     1 100%          0.198        1
#> 3 Respiratory-02 All            5           5     1 100%          0.463        1

```
