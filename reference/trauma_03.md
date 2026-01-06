# Trauma-03 Calculation

This function calculates the "Trauma-03" measure, which evaluates pain
scale reassessment for trauma patients, using a comprehensive data frame
with EMS records. The function processes input data to create both fact
and dimension tables, identifies eligible patients, and summarizes
results for adult and pediatric populations.

## Usage

``` r
trauma_03(
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
  edisposition_28_col,
  transport_disposition_col,
  evitals_01_col,
  evitals_27_col = NULL,
  evitals_27_initial_col = NULL,
  evitals_27_last_col = NULL,
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

  The column representing the EMS record unique identifier.

- incident_date_col:

  Column that contains the incident date. This defaults to `NULL` as it
  is optional in case not available due to PII restrictions.

- patient_DOB_col:

  Column that contains the patient's date of birth. This defaults to
  `NULL` as it is optional in case not available due to PII
  restrictions.

- epatient_15_col:

  The column for patient age numeric value.

- epatient_16_col:

  The column for patient age unit (e.g., "Years", "Months").

- esituation_02_col:

  The column containing information on the presence of injury.

- eresponse_05_col:

  The column representing the 911 response type.

- edisposition_28_col:

  The column for patient care disposition details.

- transport_disposition_col:

  The column for patient transport disposition.

- evitals_01_col:

  The column for the time of pain scale measurement.

- evitals_27_col:

  The column for the full set of pain scale scores.

- evitals_27_initial_col:

  The column for the initial pain scale score.

- evitals_27_last_col:

  The column for the last pain scale score.

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
# for testing a single pain scale column
  test_data2 <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    esituation_02 = rep("Yes", 5),
    evitals_01 = lubridate::as_datetime(c("2025-01-01 12:00:00", "2025-01-05
    18:00:00", "2025-02-01 06:00:00", "2025-01-01 01:00:00", "2025-06-01
    14:00:00")),
    edisposition_28 = rep(4228001, 5),
    edisposition_30 = c(4230001, 4230003, 4230001, 4230007, 4230007)
  )

  # Expand data so each erecord_01 has 2 rows (one for each pain score)
  test_data_expanded2 <- test_data2 |>
    tidyr::uncount(weights = 2) |>  # Duplicate each row twice
    # Assign pain scores
    dplyr::mutate(evitals_27 = c(0, 0, 2, 1, 4, 3, 6, 5, 8, 7)) |>
    dplyr::group_by(erecord_01) |>
    dplyr::mutate(
    # Lower score = later time
      time_offset = dplyr::if_else(dplyr::row_number() == 1, -5, 0),
      evitals_01 = evitals_01 + lubridate::dminutes(time_offset)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-time_offset)  # Remove temporary column

# Run function with the single pain score column
# Return 95% confidence intervals using the Wilson method
  trauma_03(
    df = test_data_expanded2,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    esituation_02_col = esituation_02,
    evitals_01_col = evitals_01,
    evitals_27_initial_col = NULL,
    evitals_27_last_col = NULL,
    evitals_27_col = evitals_27,
    edisposition_28_col = edisposition_28,
    transport_disposition_col = edisposition_30,
    confidence_interval = TRUE
  )
#> 
#> ── Trauma-03 ───────────────────────────────────────────────────────────────────
#> 
#> ── Gathering Records for Trauma-03 ──
#> 
#> Running `trauma_03_population()`  [Working on 1 of 17 tasks] ●●●───────────────…
#> Running `trauma_03_population()`  [Working on 2 of 17 tasks] ●●●●●─────────────…
#> Running `trauma_03_population()`  [Working on 3 of 17 tasks] ●●●●●●────────────…
#> Running `trauma_03_population()`  [Working on 4 of 17 tasks] ●●●●●●●●──────────…
#> Running `trauma_03_population()`  [Working on 5 of 17 tasks] ●●●●●●●●●●────────…
#> Running `trauma_03_population()`  [Working on 6 of 17 tasks] ●●●●●●●●●●●●──────…
#> Running `trauma_03_population()`  [Working on 7 of 17 tasks] ●●●●●●●●●●●●●─────…
#> Running `trauma_03_population()`  [Working on 8 of 17 tasks] ●●●●●●●●●●●●●●●───…
#> Running `trauma_03_population()`  [Working on 9 of 17 tasks] ●●●●●●●●●●●●●●●●●─…
#> Running `trauma_03_population()`  [Working on 10 of 17 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_03_population()`  [Working on 11 of 17 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_03_population()`  [Working on 12 of 17 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_03_population()`  [Working on 13 of 17 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_03_population()`  [Working on 17 of 17 tasks] ●●●●●●●●●●●●●●●●●…
#> 
#> 
#> 
#> ── Calculating Trauma-03 ──
#> 
#> 
#> ✔ Function completed in 0.27s.
#> 
#> Warning: In `prop.test()`: Chi-squared approximation may be incorrect for any n < 10.
#> # A tibble: 3 × 8
#>   measure   pop    numerator denominator  prop prop_label lower_ci upper_ci
#>   <chr>     <chr>      <int>       <int> <dbl> <chr>         <dbl>    <dbl>
#> 1 Trauma-03 Adults         2           2     1 100%         0.198         1
#> 2 Trauma-03 Peds           1           1     1 100%         0.0546        1
#> 3 Trauma-03 All            4           4     1 100%         0.396         1
```
