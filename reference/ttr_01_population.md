# TTR-01 Populations

Filters data down to the target populations for TTR-01, and categorizes
records to identify needed information for the calculations.

Identifies key categories to records that are 911 requests for patients
not transported by EMS during which a basic set of vital signs is
documented based on specific criteria and calculates related ECG
measures. This function segments the data by age into adult and
pediatric populations.

## Usage

``` r
ttr_01_population(
  df = NULL,
  patient_scene_table = NULL,
  response_table = NULL,
  disposition_table = NULL,
  vitals_table = NULL,
  arrest_table = NULL,
  erecord_01_col,
  incident_date_col = NULL,
  patient_DOB_col = NULL,
  epatient_15_col,
  epatient_16_col,
  eresponse_05_col,
  transport_disposition_col,
  earrest_01_col,
  evitals_06_col,
  evitals_07_col,
  evitals_10_col,
  evitals_12_col,
  evitals_14_col,
  evitals_23_col,
  evitals_26_col
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

- disposition_table:

  A data.frame or tibble containing only the edisposition fields needed
  for this measure's calculations.

- vitals_table:

  A dataframe or tibble containing at least the eVitals fields needed.

- arrest_table:

  A data.frame or tibble containing at least the eArrest fields needed
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

- eresponse_05_col:

  Column that contains eResponse.05 or the response type.

- transport_disposition_col:

  One or more unquoted column names (such as edisposition.12,
  edisposition.30) containing transport disposition for an EMS event
  identifying whether a transport occurred and by which unit.

- earrest_01_col:

  Column representing whether or not the patient is in arrest.

- evitals_06_col:

  Numeric column containing systolic blood pressure values.

- evitals_07_col:

  A column containing the patient's diastolic blood pressure.

- evitals_10_col:

  Column name containing the patient's heart rate expressed as a number
  per minute.

- evitals_12_col:

  Numeric column containing pulse oximetry values.

- evitals_14_col:

  Column name containing the patient's respiratory rate expressed as a
  number per minute.

- evitals_23_col:

  Column for Glasgow Coma Scale (GCS) scores.

- evitals_26_col:

  Column for AVPU alertness levels.

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
    eresponse_05 = rep(2205001, 5),
  )

  # arrest table
  arrest_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    earrest_01 = rep("No", 5)
  )

  # vitals table
  vitals_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    evitals_06 = c(100, 90, 80, 70, 85),
    evitals_07 = c(80, 90, 50, 60, 87),
    evitals_10 = c(110, 89, 88, 71, 85),
    evitals_12 = c(50, 60, 70, 80, 75),
    evitals_14 = c(30, 9, 8, 7, 31),
    evitals_23 = c(6, 7, 8, 9, 10),
    evitals_26 = c(3326007, 3326005, 3326003, 3326001, 3326007),
  )

  # disposition table
  disposition_table <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    edisposition_30 = c(4230013, 4230009, 4230013, 4230009, 4230013)
  )

  # test the success of the function
  result <- ttr_01_population(patient_scene_table = patient_table,
                        response_table = response_table,
                        arrest_table = arrest_table,
                        vitals_table = vitals_table,
                        disposition_table = disposition_table,
                        erecord_01_col = erecord_01,
                        incident_date_col = incident_date,
                        patient_DOB_col = patient_dob,
                        epatient_15_col = epatient_15,
                        epatient_16_col = epatient_16,
                        eresponse_05_col = eresponse_05,
                        earrest_01_col = earrest_01,
                        evitals_06_col = evitals_06,
                        evitals_07_col = evitals_07,
                        evitals_10_col = evitals_10,
                        evitals_12_col = evitals_12,
                        evitals_14_col = evitals_14,
                        evitals_23_col = evitals_23,
                        evitals_26_col = evitals_26,
                        transport_disposition_col = edisposition_30
                   )
#> Running `ttr_01_population()`  [Working on 1 of 13 tasks] ●●●──────────────────…
#> Running `ttr_01_population()`  [Working on 2 of 13 tasks] ●●●●●●───────────────…
#> Running `ttr_01_population()`  [Working on 3 of 13 tasks] ●●●●●●●●─────────────…
#> Running `ttr_01_population()`  [Working on 4 of 13 tasks] ●●●●●●●●●●───────────…
#> Running `ttr_01_population()`  [Working on 5 of 13 tasks] ●●●●●●●●●●●●●────────…
#> Running `ttr_01_population()`  [Working on 6 of 13 tasks] ●●●●●●●●●●●●●●●──────…
#> Running `ttr_01_population()`  [Working on 7 of 13 tasks] ●●●●●●●●●●●●●●●●●────…
#> Running `ttr_01_population()`  [Working on 8 of 13 tasks] ●●●●●●●●●●●●●●●●●●●──…
#> Running `ttr_01_population()`  [Working on 9 of 13 tasks] ●●●●●●●●●●●●●●●●●●●●●…
#> Running `ttr_01_population()`  [Working on 10 of 13 tasks] ●●●●●●●●●●●●●●●●●●●●…
#> Running `ttr_01_population()`  [Working on 13 of 13 tasks] ●●●●●●●●●●●●●●●●●●●●…
#> 

# show the results of filtering at each step
result$filter_process
#> # A tibble: 8 × 2
#>   filter                                           count
#>   <chr>                                            <int>
#> 1 911 calls                                            5
#> 2 Non-transports                                       5
#> 3 Non-cardiac arrest                                   5
#> 4 Non-null SBP, DBP, HR, SPO2, RR, and GCS or AVPU     5
#> 5 Adults denominator                                   2
#> 6 Peds denominator                                     3
#> 7 Initial population                                   5
#> 8 Total dataset                                        5
```
