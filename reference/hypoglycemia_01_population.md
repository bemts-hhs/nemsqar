# Hypoglycemia-01 Populations

Filters data down to the target populations for Hypoglycemia-01, and
categorizes records to identify needed information for the calculations.

Identifies key categories related to diabetes/hypoglycemia incidents in
an EMS dataset, specifically focusing on cases where 911 was called for
diabetes/hypoglycemia distress, certain medications were administered,
and a weight is taken. This function segments the data into pediatric
populations, computing the proportion of cases that have a documented
weight.

## Usage

``` r
hypoglycemia_01_population(
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
  eprocedures_03_col
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

- esituation_11_col:

  Column that contains eSituation.11 provider primary impression data.

- esituation_12_col:

  Column that contains all eSituation.12 values as (possible a single
  comma-separated list), provider secondary impression data.

- evitals_18_col:

  Column for blood glucose levels.

- evitals_23_col:

  Column for Glasgow Coma Scale (GCS) scores.

- evitals_26_col:

  Column for AVPU alertness levels.

- emedications_03_col:

  Column that contains all medication administered to the patient
  (eMedications.03) values as a single comma-separated list per distinct
  eRecord.01 ID.

- eprocedures_03_col:

  Column containing procedure codes with or without procedure names.

## Value

A list that contains the following:

- a tibble with counts for each filtering step,

- a tibble for each population of interest

- a tibble for the initial population

- a tibble for the total dataset with computations

## Author

Nicolas Foss, Ed.D., MS

## Examples

``` r
# create tables to test correct functioning

# patient table
patient_table <- tibble::tibble(

  erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
  incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01",
  "2025-01-01", "2025-06-01")),
  patient_dob = as.Date(c("2000-01-01", "2020-01-01", "2023-02-01",
  "2023-01-01", "1970-06-01")),
  epatient_15 = c(25, 5, 2, 2, 55),  # Ages
  epatient_16 = c("Years", "Years", "Years", "Years", "Years")

)

# response table
response_table <- tibble::tibble(

  erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
  eresponse_05 = rep(2205001, 5)

)

# situation table
situation_table <- tibble::tibble(

  erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
  esituation_11 = c(rep("E13.64", 3), rep("E16.2", 2)),
  esituation_12 = c(rep("E13.64", 2), rep("E16.2", 3))
)

# medications table
medications_table <- tibble::tibble(

  erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
  emedications_03 = c(372326, 376937, 377980, 4850, 4832),

)

# vitals table
vitals_table <- tibble::tibble(

  erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
  evitals_18 = c(60, 59, 58, 57, 56),
  evitals_23 = c(16, 15, 14, 13, 12),
  evitals_26 = c("Alert", "Painful", "Verbal", "Unresponsive", "Alert")

)

# procedures table
procedures_table <- tibble::tibble(

  erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
  eprocedures_03 = rep("710925007", 5)

)

# test the success of the function
result <- hypoglycemia_01_population(patient_scene_table = patient_table,
                            response_table = response_table,
                            situation_table = situation_table,
                            medications_table = medications_table,
                            vitals_table = vitals_table,
                            procedures_table = procedures_table,
                            erecord_01_col = erecord_01,
                            incident_date_col = incident_date,
                            patient_DOB_col = patient_dob,
                            epatient_15_col = epatient_15,
                            epatient_16_col = epatient_16,
                            eresponse_05_col = eresponse_05,
                            esituation_11_col = esituation_11,
                            esituation_12_col = esituation_12,
                            emedications_03_col = emedications_03,
                            evitals_18_col = evitals_18,
                            evitals_23_col = evitals_23,
                            evitals_26_col = evitals_26,
                            eprocedures_03_col = eprocedures_03
                            )
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

# show the results of filtering at each step
result$filter_process
#> # A tibble: 7 × 2
#>   filter                                                              count
#>   <chr>                                                               <int>
#> 1 Diabetes/Hypoglycemia and Verbal, Painful, Unresponsive or GCS < 15     4
#> 2 Altered mental status and low blood glucose                             0
#> 3 911 calls                                                               5
#> 4 Adults denominator                                                      1
#> 5 Peds denominator                                                        3
#> 6 Initial population                                                      4
#> 7 Total dataset                                                           5
```
