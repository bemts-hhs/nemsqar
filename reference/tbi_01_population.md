# TBI-01 Populations

This function screens for potential traumatic brain injury (TBI) cases
based on specific criteria in a patient dataset. It produces a subset of
the data with calculated variables for TBI identification.

## Usage

``` r
tbi_01_population(
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
  eresponse_05_col,
  esituation_11_col,
  esituation_12_col,
  transport_disposition_col,
  evitals_06_col,
  evitals_12_col,
  evitals_16_col,
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

- situation_table:

  A data.frame or tibble containing at least the eSituation fields
  needed for this measure's calculations. Default is `NULL`.

- disposition_table:

  A data.frame or tibble containing only the edisposition fields needed
  for this measure's calculations.

- vitals_table:

  A dataframe or tibble containing at least the eVitals fields needed.

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

- transport_disposition_col:

  One or more unquoted column names (such as edisposition.12,
  edisposition.30) containing transport disposition for an EMS event
  identifying whether a transport occurred and by which unit.

- evitals_06_col:

  Numeric column containing systolic blood pressure values.

- evitals_12_col:

  Numeric column containing pulse oximetry values.

- evitals_16_col:

  Column with numeric value of the patient's exhaled end tidal carbon
  dioxide (ETCO2) level measured as a unit of pressure in millimeters of
  mercury (mmHg), percentage or, kilopascal (kPa).

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
    incident_date = as.Date(c("2025-01-01", "2025-01-05",
                              "2025-02-01", "2025-01-01",
                              "2025-06-01")
                              ),
    patient_dob = as.Date(c("2000-01-01", "2020-01-01",
                            "2023-02-01", "2023-01-01",
                            "1970-06-01")
                            ),
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
    esituation_11 = c(rep("S02", 3), rep("S06", 2)),
    esituation_12 = c(rep("S09.90", 2), rep("S06.0X9", 3)),
  )


  # vitals table
  vitals_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    evitals_06 = c(85, 80, 100, 90, 82),
    evitals_12 = c(95, 96, 97, 98, 99),
    evitals_16 = c(35, 36, 37, 38, 39),
    evitals_23 = rep(8, 5),
    evitals_26 = c("Verbal", "Painful", "Unresponsive", "Verbal", "Painful")
  )

  # disposition table
  disposition_table <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    edisposition_30 = c(4230001, 4230003, 4230001, 4230007, 4230007)
  )

# test the success of the function
  result <- tbi_01_population(patient_scene_table = patient_table,
                         response_table = response_table,
                         situation_table = situation_table,
                         vitals_table = vitals_table,
                         disposition_table = disposition_table,
                         erecord_01_col = erecord_01,
                         incident_date_col = NULL,
                         patient_DOB_col = NULL,
                         epatient_15_col = epatient_15,
                         epatient_16_col = epatient_16,
                         eresponse_05_col = eresponse_05,
                         esituation_11_col = esituation_11,
                         esituation_12_col = esituation_12,
                         evitals_06_col = evitals_06,
                         evitals_12_col = evitals_12,
                         evitals_16_col = evitals_16,
                         evitals_23_col = evitals_23,
                         evitals_26_col = evitals_26,
                         transport_disposition_col = edisposition_30
                     )
#> Running `tbi_01_population()`  [Working on 1 of 15 tasks] ●●●──────────────────…
#> Running `tbi_01_population()`  [Working on 2 of 15 tasks] ●●●●●────────────────…
#> Running `tbi_01_population()`  [Working on 5 of 15 tasks] ●●●●●●●●●●●──────────…
#> Running `tbi_01_population()`  [Working on 6 of 15 tasks] ●●●●●●●●●●●●●────────…
#> Running `tbi_01_population()`  [Working on 7 of 15 tasks] ●●●●●●●●●●●●●●●──────…
#> Running `tbi_01_population()`  [Working on 8 of 15 tasks] ●●●●●●●●●●●●●●●●●────…
#> Running `tbi_01_population()`  [Working on 9 of 15 tasks] ●●●●●●●●●●●●●●●●●●●──…
#> Running `tbi_01_population()`  [Working on 10 of 15 tasks] ●●●●●●●●●●●●●●●●●●●●…
#> Running `tbi_01_population()`  [Working on 12 of 15 tasks] ●●●●●●●●●●●●●●●●●●●●…
#> Running `tbi_01_population()`  [Working on 13 of 15 tasks] ●●●●●●●●●●●●●●●●●●●●…
#> Running `tbi_01_population()`  [Working on 14 of 15 tasks] ●●●●●●●●●●●●●●●●●●●●…
#> Running `tbi_01_population()`  [Working on 15 of 15 tasks] ●●●●●●●●●●●●●●●●●●●●…
#> 

# show the results of filtering at each step
result$filter_process
#> # A tibble: 10 × 2
#>    filter                                   count
#>    <chr>                                    <int>
#>  1 911 calls                                    5
#>  2 TBI cases                                    5
#>  3 GCS < 15                                     5
#>  4 AVPU is verbal, painful, or unresponsive     5
#>  5 Transports                                   5
#>  6 Oxygen level, ETC02, SBP are documented      5
#>  7 Adults denominator                           2
#>  8 Peds denominator                             3
#>  9 Initial population                           5
#> 10 Total dataset                                5
```
