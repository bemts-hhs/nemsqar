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

- exam_table:

  A data.frame or tibble containing only the eExam fields needed for
  this measure's calculations. Default is `NULL`.

- procedures_table:

  A dataframe or tibble containing at least the eProcedures fields
  needed.

- injury_table:

  A data frame or tibble containing fields from eInjury needed for this
  measure's calculations.

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

  Column representing the patient's numeric age agnostic of unit.

- epatient_16_col:

  Column representing the patient's age unit ("Years", "Months", "Days",
  "Hours", or "Minutes").

- esituation_02_col:

  Column indicating whether or not there was an injury.

- eresponse_05_col:

  Column that contains eResponse.05 or the response type.

- eresponse_10_col:

  Column name containing informatin about scene delays, if any, of the
  EMS unit associated with the EMS event.

- transport_disposition_col:

  One or more unquoted column names (such as edisposition.12,
  edisposition.30) containing transport disposition for an EMS event
  identifying whether a transport occurred and by which unit.

- edisposition_23_col:

  Column name containing primary hospital capability associated with the
  patient's condition for this transport (e.g., Trauma, STEMI, Peds,
  etc.).

- evitals_06_col:

  Numeric column containing systolic blood pressure values.

- evitals_10_col:

  Column name containing the patient's heart rate expressed as a number
  per minute.

- evitals_12_col:

  Numeric column containing pulse oximetry values.

- evitals_14_col:

  Column name containing the patient's respiratory rate expressed as a
  number per minute.

- evitals_15_col:

  Column name containing the patient's respiratory effort.

- evitals_21_col:

  Column name containing the patient's Glasgow Coma Score Motor
  response.

- eexam_16_col:

  Column name containing the assessment findings associated with the
  patient's extremities.

- eexam_20_col:

  Column name containing the assessment findings of the patient's
  neurological examination.

- eexam_23_col:

  Column name containing the assessment findings associated with the
  patient's lungs.

- eexam_25_col:

  Column name containing the assessment findings associated with the
  patient's chest.

- eprocedures_03_col:

  Column containing procedure codes with or without procedure names.

- einjury_01_col:

  Column name containing the category of the reported/suspected external
  cause of the injury.

- einjury_03_col:

  Column describing Trauma triage criteria for the red boxes (Injury
  Patterns and Mental Status and Vital Signs) in the 2021 ACS National
  Guideline for the Field Triage of Injured Patients.

- einjury_04_col:

  Column name containing Trauma triage criteria for the yellow boxes
  (Mechanism of Injury and EMS Judgment) in the current ACS National
  Guideline for the Field Triage of Injured Patients.

- einjury_09_col:

  Column name containing the distance in feet the patient fell, measured
  from the lowest point of the patient to the ground.

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
    incident_date_col = NULL,
    patient_DOB_col = NULL,
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
#> ✔ Function completed in 0.49s.
#> 
#> Warning: In `prop.test()`: Chi-squared approximation may be incorrect for any n < 10.
#> # A tibble: 3 × 8
#>   measure   pop       numerator denominator  prop prop_label lower_ci upper_ci
#>   <chr>     <chr>         <int>       <int> <dbl> <chr>         <dbl>    <dbl>
#> 1 Trauma-04 >= 65 yrs         0           0   NaN NA          NaN          NaN
#> 2 Trauma-04 10-64 yrs         3           3     1 100%          0.310        1
#> 3 Trauma-04 < 10 yrs          2           2     1 100%          0.198        1

```
