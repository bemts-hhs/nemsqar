# Asthma-01 Calculation

Calculates the NEMSQA Asthma-01 measure.

Calculates key statistics related to asthma-related incidents in an EMS
dataset, specifically focusing on cases where 911 was called for
respiratory distress, and certain medications were administered. This
function segments the data by age into adult and pediatric populations,
computing the proportion of cases that received beta-agonist treatment.

## Usage

``` r
asthma_01(
  df = NULL,
  patient_scene_table = NULL,
  response_table = NULL,
  situation_table = NULL,
  medications_table = NULL,
  erecord_01_col,
  incident_date_col = NULL,
  patient_DOB_col = NULL,
  epatient_15_col,
  epatient_16_col,
  eresponse_05_col,
  esituation_11_col,
  esituation_12_col,
  emedications_03_col,
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

- medications_table:

  A data.frame or tibble containing at least the eMedications fields
  needed for this measure's calculations. Default is `NULL`.

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

- emedications_03_col:

  Column that contains all medication administered to the patient
  (eMedications.03) values as a single comma-separated list per distinct
  eRecord.01 ID.

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
  esituation_11 = c("Respiratory Distress", "Respiratory Distress",
  "Chest Pain", "Respiratory Distress", "Respiratory Distress"),
  esituation_12 = c("Asthma", "Asthma", "Other condition", "Asthma", "Asthma"),
  emedications_03 = c("Albuterol", "Albuterol", "Epinephrine", "None",
  "Albuterol")
)

# Run the function
# Return 95% confidence intervals using the Wilson method
asthma_01(
  df = test_data,
  erecord_01_col = erecord_01,
  incident_date_col = NULL,
  patient_DOB_col = NULL,
  epatient_15_col = epatient_15,
  epatient_16_col = epatient_16,
  eresponse_05_col = eresponse_05,
  esituation_11_col = esituation_11,
  esituation_12_col = esituation_12,
  emedications_03_col = emedications_03,
  confidence_interval = TRUE
)
#> 
#> ── Asthma-01 ───────────────────────────────────────────────────────────────────
#> 
#> ── Gathering Records for Asthma-01 ──
#> 
#> Running `asthma_01_population()`  [Working on 1 of 10 tasks] ●●●●──────────────…
#> Running `asthma_01_population()`  [Working on 2 of 10 tasks] ●●●●●●●───────────…
#> Running `asthma_01_population()`  [Working on 3 of 10 tasks] ●●●●●●●●●●────────…
#> Running `asthma_01_population()`  [Working on 4 of 10 tasks] ●●●●●●●●●●●●●─────…
#> Running `asthma_01_population()`  [Working on 5 of 10 tasks] ●●●●●●●●●●●●●●●●──…
#> Running `asthma_01_population()`  [Working on 6 of 10 tasks] ●●●●●●●●●●●●●●●●●●…
#> Running `asthma_01_population()`  [Working on 7 of 10 tasks] ●●●●●●●●●●●●●●●●●●…
#> Running `asthma_01_population()`  [Working on 8 of 10 tasks] ●●●●●●●●●●●●●●●●●●…
#> Running `asthma_01_population()`  [Working on 9 of 10 tasks] ●●●●●●●●●●●●●●●●●●…
#> Running `asthma_01_population()`  [Working on 10 of 10 tasks] ●●●●●●●●●●●●●●●●●…
#> 
#> 
#> 
#> ── Calculating Asthma-01 ──
#> 
#> 
#> ✔ Function completed in 0.15s.
#> 
#> Warning: In `prop.test()`: Chi-squared approximation may be incorrect for any n < 10.
#> # A tibble: 3 × 8
#>   measure   pop    numerator denominator  prop prop_label lower_ci upper_ci
#>   <chr>     <chr>      <int>       <int> <dbl> <chr>         <dbl>    <dbl>
#> 1 Asthma-01 Adults         2           2  1    100%         0.198     1    
#> 2 Asthma-01 Peds           1           1  1    100%         0.0546    1    
#> 3 Asthma-01 All            3           4  0.75 75%          0.219     0.987
```
