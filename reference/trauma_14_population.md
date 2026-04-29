# Trauma-14 Population

This function processes EMS data to generate the population needed to
calculate the Trauma-14 NEMSQA measure.

## Usage

``` r
trauma_14_population(
  df = NULL,
  patient_scene_table = NULL,
  situation_table = NULL,
  response_table = NULL,
  disposition_table = NULL,
  vitals_table = NULL,
  exam_table = NULL,
  procedures_table = NULL,
  injury_table = NULL,
  erecord_01_col,
  incident_date_col = NULL,
  patient_DOB_col = NULL,
  epatient_15_col,
  epatient_16_col,
  esituation_02_col,
  eresponse_05_col,
  eresponse_10_col,
  transport_disposition_col,
  edisposition_24_col,
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
  einjury_09_col
)
```

## Arguments

- df:

  A dataframe or tibble contianing EMS data where each row represents an
  observation and columns represent features.

- patient_scene_table:

  A data.frame or tibble containing at least ePatient, and eScene as a
  fact table.

- situation_table:

  A data.frame or tibble containing at least the eSituation fields
  needed for this measure's calculations. Default is `NULL`.

- response_table:

  A data.frame or tibble containing at least the eResponse fields needed
  for this measure's calculations.

- disposition_table:

  A data.frame or tibble containing only the edisposition fields needed
  for this measure's calculations.

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

- edisposition_24_col:

  Column name containing the indication that an alert (or activation)
  was called by EMS to the appropriate destination healthcare facility
  team. The alert (or activation) should occur prior to the EMS Unit
  arrival at the destination with the patient.

- evitals_06_col:

  Numeric column containing systolic blood pressure values.

- evitals_10_col:

  Column name containing the patient's heart rate expressed as a number
  per minute.

- evitals_12_col:

  Numeric column containing pulse oximetry values.

- evitals_14_col:

  Column containing data on patient's respiratory rate expressed as a
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
    eresponse_10 = rep(2210011, 5)
  )

  # situation table
  situation_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    esituation_02 = rep("Yes", 5),
  )

  # vitals table
  vitals_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    evitals_06 = c(100, 90, 80, 70, 85),
    evitals_10 = c(110, 89, 88, 71, 85),
    evitals_12 = c(50, 60, 70, 80, 75),
    evitals_14 = c(30, 9, 8, 7, 31),
    evitals_15 = c("apneic", "labored", "rapid", "shallow", "weak/agonal"),
    evitals_21 = c(5, 4, 3, 2, 1)
  )

  # disposition table
  disposition_table <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    edisposition_24 = c(4224017, 4224003, 4224017, 4224003, 4224017),
    edisposition_30 = c(4230001, 4230003, 4230001, 4230007, 4230007)
  )

  # injury table
  injury_table <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    einjury_01 = c("V20", "V36", "V86", "V39", "V32"),
    einjury_03 = c(2903011, 2903009, 2903005, 3903003, 2903001),
    einjury_04 = c(2904013, 2904011, 2904009, 2904007, 2904001),
    einjury_09 = c(11, 12, 13, 14, 15)
  )

  # exam table
  exam_table <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    eexam_16 = c(3516043, 3516067, 3516043, 3516067, 3516067),
    eexam_20 = c(3520045, 3520043, 3520019, 3520017, 3520017),
    eexam_23 = c(3523011, 3523003, 3523001, 3523011, 3523003),
    eexam_25 = c(3525039, 3525023, 3525005, 3525039, 3525023)
  )

  # procedures table
  procedures_table <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    eprocedures_03 = c(424979004, 427753009, 429705000, 47545007, 243142003)
  )

  # test the success of the function
  result <- trauma_14_population(patient_scene_table = patient_table,
                      response_table = response_table,
                      situation_table = situation_table,
                      vitals_table = vitals_table,
                      disposition_table = disposition_table,
                      exam_table = exam_table,
                      injury_table = injury_table,
                      procedures_table = procedures_table,
                      erecord_01_col = erecord_01,
                      incident_date_col = incident_date,
                      patient_DOB_col = patient_dob,
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
                      edisposition_24_col = edisposition_24,
                      transport_disposition_col = edisposition_30,
                      eprocedures_03_col = eprocedures_03,
                      einjury_01_col = einjury_01,
                      einjury_03_col = einjury_03,
                      einjury_04_col = einjury_04,
                      einjury_09_col = einjury_09
  )
#> Running `trauma_14_population()`  [Working on 1 of 33 tasks] ●●────────────────…
#> Running `trauma_14_population()`  [Working on 2 of 33 tasks] ●●●───────────────…
#> Running `trauma_14_population()`  [Working on 3 of 33 tasks] ●●●●──────────────…
#> Running `trauma_14_population()`  [Working on 4 of 33 tasks] ●●●●●─────────────…
#> Running `trauma_14_population()`  [Working on 5 of 33 tasks] ●●●●●●────────────…
#> Running `trauma_14_population()`  [Working on 6 of 33 tasks] ●●●●●●────────────…
#> Running `trauma_14_population()`  [Working on 7 of 33 tasks] ●●●●●●●───────────…
#> Running `trauma_14_population()`  [Working on 8 of 33 tasks] ●●●●●●●●──────────…
#> Running `trauma_14_population()`  [Working on 9 of 33 tasks] ●●●●●●●●●─────────…
#> Running `trauma_14_population()`  [Working on 10 of 33 tasks] ●●●●●●●●●●───────…
#> Running `trauma_14_population()`  [Working on 11 of 33 tasks] ●●●●●●●●●●●──────…
#> Running `trauma_14_population()`  [Working on 12 of 33 tasks] ●●●●●●●●●●●●─────…
#> Running `trauma_14_population()`  [Working on 13 of 33 tasks] ●●●●●●●●●●●●●────…
#> Running `trauma_14_population()`  [Working on 14 of 33 tasks] ●●●●●●●●●●●●●●───…
#> Running `trauma_14_population()`  [Working on 15 of 33 tasks] ●●●●●●●●●●●●●●●──…
#> Running `trauma_14_population()`  [Working on 16 of 33 tasks] ●●●●●●●●●●●●●●●●─…
#> Running `trauma_14_population()`  [Working on 17 of 33 tasks] ●●●●●●●●●●●●●●●●─…
#> Running `trauma_14_population()`  [Working on 18 of 33 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_14_population()`  [Working on 19 of 33 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_14_population()`  [Working on 20 of 33 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_14_population()`  [Working on 21 of 33 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_14_population()`  [Working on 22 of 33 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_14_population()`  [Working on 23 of 33 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_14_population()`  [Working on 24 of 33 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_14_population()`  [Working on 25 of 33 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_14_population()`  [Working on 26 of 33 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_14_population()`  [Working on 27 of 33 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_14_population()`  [Working on 28 of 33 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_14_population()`  [Working on 29 of 33 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_14_population()`  [Working on 30 of 33 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_14_population()`  [Working on 31 of 33 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_14_population()`  [Working on 32 of 33 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_14_population()`  [Working on 33 of 33 tasks] ●●●●●●●●●●●●●●●●●…
#> 

# show the results of filtering at each step
result$filter_process
#> # A tibble: 30 × 2
#>    filter                                                                  count
#>    <chr>                                                                   <int>
#>  1 Situation possible injury                                                   5
#>  2 911 calls                                                                   5
#>  3 Transports                                                                  5
#>  4 GCS Motor 1-5                                                               5
#>  5 Breath sounds absent, decreased, increased respiratory effort               5
#>  6 Flail segment, retraction, accessory muscles used in breathing              5
#>  7 Respiratory effort apneic, labored, mech. assist, rapid, shallow, weak…     5
#>  8 Airway management procedures                                                5
#>  9 Pulse oximetry < 90                                                         5
#> 10 SBP < 110                                                                   5
#> # ℹ 20 more rows
```
