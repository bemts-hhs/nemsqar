# Trauma-04 Calculations

This function processes EMS data to generate a set of binary variables
indicating whether specific trauma triage criteria are met. The output
is a data frame enriched with these indicators for further analysis. The
final outcome is whether or not the EMS record documents the use of a
verified trauma center levels 1-5 in the hospital capability
documentation.

## Usage

``` r
trauma_04(
  df = NULL,
  patient_scene_table = NULL,
  response_table = NULL,
  situation_table = NULL,
  vitals_table = NULL,
  exam_table = NULL,
  procedures_table = NULL,
  injury_table = NULL,
  disposition_table = NULL,
  erecord_01_col,
  incident_date_col = NULL,
  patient_DOB_col = NULL,
  epatient_15_col,
  epatient_16_col,
  esituation_02_col,
  eresponse_05_col,
  eresponse_10_col,
  transport_disposition_col,
  edisposition_23_col,
  evitals_06_col,
  evitals_10_col,
  evitals_12_col,
  evitals_14_col,
  evitals_15_col,
  evitals_21_col,
  eexam_16_col,
  eexam_20_col,
  eexam_23_col,
  eexam_25_col,
  eprocedures_03_col,
  einjury_01_col,
  einjury_03_col,
  einjury_04_col,
  einjury_09_col,
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

- patient_scene_table:

  A data.frame or tibble containing only epatient and escene fields as a
  fact table.

- response_table:

  A data.frame or tibble containing only the eresponse fields needed for
  this measure's calculations.

- situation_table:

  A data.frame or tibble containing only the esituation fields needed
  for this measure's calculations.

- vitals_table:

  A data.frame or tibble containing only the evitals fields needed for
  this measure's calculations.

- exam_table:

  A data.frame or tibble containing only the eexam fields needed for
  this measure's calculations.

- procedures_table:

  A data.frame or tibble containing only the eprocedures fields needed
  for this measure's calculations.

- injury_table:

  A data.frame or tibble containing only the einjury fields needed for
  this measure's calculations.

- disposition_table:

  A data.frame or tibble containing only the edisposition fields needed
  for this measure's calculations.

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

- eresponse_10_col:

  Column name containing scene delay information.

- transport_disposition_col:

  The column for patient transport disposition.

- edisposition_23_col:

  Column name containing trauma hospital verification information.

- evitals_06_col:

  Column name containing systolic blood pressure (SBP) values.

- evitals_10_col:

  Column name containing heart rate values.

- evitals_12_col:

  Column name containing pulse oximetry values.

- evitals_14_col:

  Column name containing capillary refill information.

- evitals_15_col:

  Column name containing respiratory effort values.

- evitals_21_col:

  Column name containing Glasgow Coma Scale (GCS) Motor values.

- eexam_16_col:

  Column name containing extremities assessment details.

- eexam_20_col:

  Column name containing neurological assessment details.

- eexam_23_col:

  Column name containing lung assessment details.

- eexam_25_col:

  Column name containing chest assessment details.

- eprocedures_03_col:

  Column name containing airway management or tourniquet usage details.

- einjury_01_col:

  Column name containing injury cause details.

- einjury_03_col:

  Column name containing trauma triage steps 1 and 2 information.

- einjury_04_col:

  Column name containing trauma triage steps 3 and 4 information.

- einjury_09_col:

  Column name containing fall height information.

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
    eresponse_10 = rep(2210011, 5),
    esituation_02 = rep("Yes", 5),
    evitals_06 = c(100, 90, 80, 70, 85),
    evitals_10 = c(110, 89, 88, 71, 85),
    evitals_12 = c(50, 60, 70, 80, 75),
    evitals_14 = c(30, 9, 8, 7, 31),
    evitals_15 = c("apneic", "labored", "rapid", "shallow", "weak/agonal"),
    evitals_21 = c(5, 4, 3, 2, 1),
    eexam_16 = c(3516043, 3516067, 3516043, 3516067, 3516067),
    eexam_20 = c(3520045, 3520043, 3520019, 3520017, 3520017),
    eexam_23 = c(3523011, 3523003, 3523001, 3523011, 3523003),
    eexam_25 = c(3525039, 3525023, 3525005, 3525039, 3525023),
    edisposition_23 = c(9908029, 9908027, 9908025, 9908023, 9908021),
    edisposition_30 = c(4230001, 4230003, 4230001, 4230007, 4230007),
    eprocedures_03 = c(424979004, 427753009, 429705000, 47545007, 243142003),
    einjury_01 = c("V20", "V36", "V86", "V39", "V32"),
    einjury_03 = c(2903011, 2903009, 2903005, 3903003, 2903001),
    einjury_04 = c(2904013, 2904011, 2904009, 2904007, 2904001),
    einjury_09 = c(11, 12, 13, 14, 15)
  )

  # Run function with the first and last pain score columns
  # Return 95% confidence intervals using the Wilson method
  trauma_04(
    df = test_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    eresponse_10_col = eresponse_10,
    esituation_02_col = esituation_02,
    evitals_06_col = evitals_06,
    evitals_10_col = evitals_10,
    evitals_12_col = evitals_12,
    evitals_14_col = evitals_14,
    evitals_15_col = evitals_15,
    evitals_21_col = evitals_21,
    eexam_16_col = eexam_16,
    eexam_20_col = eexam_20,
    eexam_23_col = eexam_23,
    eexam_25_col = eexam_25,
    edisposition_23_col = edisposition_23,
    transport_disposition_col = edisposition_30,
    eprocedures_03_col = eprocedures_03,
    einjury_01_col = einjury_01,
    einjury_03_col = einjury_03,
    einjury_04_col = einjury_04,
    einjury_09_col = einjury_09,
    confidence_interval = TRUE
  )
#> 
#> ── Trauma-04 ───────────────────────────────────────────────────────────────────
#> 
#> ── Gathering Records for Trauma-04 ──
#> 
#> Running `trauma_04_population()`  [Working on 1 of 31 tasks] ●●────────────────…
#> Running `trauma_04_population()`  [Working on 2 of 31 tasks] ●●●───────────────…
#> Running `trauma_04_population()`  [Working on 3 of 31 tasks] ●●●●──────────────…
#> Running `trauma_04_population()`  [Working on 4 of 31 tasks] ●●●●●─────────────…
#> Running `trauma_04_population()`  [Working on 5 of 31 tasks] ●●●●●●────────────…
#> Running `trauma_04_population()`  [Working on 6 of 31 tasks] ●●●●●●●───────────…
#> Running `trauma_04_population()`  [Working on 7 of 31 tasks] ●●●●●●●●──────────…
#> Running `trauma_04_population()`  [Working on 8 of 31 tasks] ●●●●●●●●●─────────…
#> Running `trauma_04_population()`  [Working on 9 of 31 tasks] ●●●●●●●●●●────────…
#> Running `trauma_04_population()`  [Working on 10 of 31 tasks] ●●●●●●●●●●●──────…
#> Running `trauma_04_population()`  [Working on 11 of 31 tasks] ●●●●●●●●●●●●─────…
#> Running `trauma_04_population()`  [Working on 12 of 31 tasks] ●●●●●●●●●●●●●────…
#> Running `trauma_04_population()`  [Working on 13 of 31 tasks] ●●●●●●●●●●●●●●───…
#> Running `trauma_04_population()`  [Working on 14 of 31 tasks] ●●●●●●●●●●●●●●●──…
#> Running `trauma_04_population()`  [Working on 15 of 31 tasks] ●●●●●●●●●●●●●●●●─…
#> Running `trauma_04_population()`  [Working on 16 of 31 tasks] ●●●●●●●●●●●●●●●●─…
#> Running `trauma_04_population()`  [Working on 17 of 31 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_04_population()`  [Working on 18 of 31 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_04_population()`  [Working on 19 of 31 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_04_population()`  [Working on 20 of 31 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_04_population()`  [Working on 21 of 31 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_04_population()`  [Working on 22 of 31 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_04_population()`  [Working on 23 of 31 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_04_population()`  [Working on 24 of 31 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_04_population()`  [Working on 25 of 31 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_04_population()`  [Working on 26 of 31 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_04_population()`  [Working on 27 of 31 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_04_population()`  [Working on 28 of 31 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_04_population()`  [Working on 29 of 31 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_04_population()`  [Working on 30 of 31 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_04_population()`  [Working on 31 of 31 tasks] ●●●●●●●●●●●●●●●●●…
#> 
#> 
#> 
#> ── Calculating Trauma-04 ──
#> 
#> 
#> ✔ Function completed in 0.47s.
#> 
#> Warning: In `prop.test()`: Chi-squared approximation may be incorrect for any n < 10.
#> # A tibble: 3 × 8
#>   measure   pop       numerator denominator  prop prop_label lower_ci upper_ci
#>   <chr>     <chr>         <int>       <int> <dbl> <chr>         <dbl>    <dbl>
#> 1 Trauma-04 >= 65 yrs         0           0   NaN NA          NaN          NaN
#> 2 Trauma-04 10-64 yrs         3           3     1 100%          0.310        1
#> 3 Trauma-04 < 10 yrs          2           2     1 100%          0.198        1

```
