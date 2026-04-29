# Trauma-08 Populations

Filters data down to the target populations for Trauma-08, and
categorizes records to identify needed information for the calculations.

Identifies key categories to records that are 911 requests for patients
with trauma during which GCS, systolic blood pressure, and respiratory
rate are documented based on specific criteria and calculates related
ECG measures. This function segments the data by age into adult and
pediatric populations.

## Usage

``` r
trauma_08_population(
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
  transport_disposition_col,
  evitals_06_col,
  evitals_14_col,
  evitals_23_col
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

- esituation_02_col:

  Column indicating whether or not there was an injury.

- eresponse_05_col:

  Column that contains eResponse.05 or the response type.

- transport_disposition_col:

  One or more unquoted column names (such as edisposition.12,
  edisposition.30) containing transport disposition for an EMS event
  identifying whether a transport occurred and by which unit.

- evitals_06_col:

  Numeric column containing systolic blood pressure values.

- evitals_14_col:

  Column containing data on patient's respiratory rate expressed as a
  number per minute.

- evitals_23_col:

  Column for Glasgow Coma Scale (GCS) scores.

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
    esituation_02 = rep("Yes", 5)
  )

  # vitals table
  vitals_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    evitals_06 = c(100, 90, 80, 70, 85),
    evitals_14 = c(30, 9, 8, 7, 31),
    evitals_23 = c(6, 7, 8, 8, 7),
  )

  # disposition table
  disposition_table <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    edisposition_30 = c(4230001, 4230003, 4230001, 4230007, 4230007)
  )

# test the success of the function
  result <- trauma_08_population(patient_scene_table = patient_table,
                      response_table = response_table,
                      situation_table = situation_table,
                      vitals_table = vitals_table,
                      disposition_table = disposition_table,
                      erecord_01_col = erecord_01,
                      incident_date_col = incident_date,
                      patient_DOB_col = patient_dob,
                      epatient_15_col = epatient_15,
                      epatient_16_col = epatient_16,
                      eresponse_05_col = eresponse_05,
                      esituation_02_col = esituation_02,
                      evitals_06_col = evitals_06,
                      evitals_14_col = evitals_14,
                      evitals_23_col = evitals_23,
                      transport_disposition_col = edisposition_30
                      )
#> Running `trauma_08_population()`  [Working on 1 of 12 tasks] ●●●───────────────…
#> Running `trauma_08_population()`  [Working on 2 of 12 tasks] ●●●●●●────────────…
#> Running `trauma_08_population()`  [Working on 3 of 12 tasks] ●●●●●●●●●─────────…
#> Running `trauma_08_population()`  [Working on 4 of 12 tasks] ●●●●●●●●●●●───────…
#> Running `trauma_08_population()`  [Working on 7 of 12 tasks] ●●●●●●●●●●●●●●●●●●…
#> Running `trauma_08_population()`  [Working on 8 of 12 tasks] ●●●●●●●●●●●●●●●●●●…
#> Running `trauma_08_population()`  [Working on 9 of 12 tasks] ●●●●●●●●●●●●●●●●●●…
#> Running `trauma_08_population()`  [Working on 10 of 12 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_08_population()`  [Working on 11 of 12 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_08_population()`  [Working on 12 of 12 tasks] ●●●●●●●●●●●●●●●●●…
#> 

# show the results of filtering at each step
result$filter_process
#> # A tibble: 8 × 2
#>   filter                    count
#>   <chr>                     <int>
#> 1 911 calls                     5
#> 2 Transports                    5
#> 3 Injury cases                  5
#> 4 Non-null RR, SBP, and GCS     5
#> 5 Adults denominator            2
#> 6 Peds denominator              3
#> 7 Initial population            5
#> 8 Total dataset                 5
```
