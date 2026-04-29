# Seizure-02 Populations

Filters data down to the target populations for Seizure-02, and
categorizes records to identify needed information for the calculations.

Identifies key categories related to asthma-related incidents in an EMS
dataset, specifically focusing on cases where 911 was called for
respiratory distress, and certain medications were administered. This
function segments the data by age into adult and pediatric populations.

## Usage

``` r
seizure_02_population(
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
  emedications_03_col
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
    esituation_11 = rep("G40", 5),
    esituation_12 = rep("r56", 5),
  )

  # medications table
  medications_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    emedications_03 = rep(3322, 5)

  )

  # test the success of the function
  result <- seizure_02_population(patient_scene_table = patient_table,
                              response_table = response_table,
                              situation_table = situation_table,
                              medications_table = medications_table,
                              erecord_01_col = erecord_01,
                              incident_date_col = incident_date,
                              patient_DOB_col = patient_dob,
                              epatient_15_col = epatient_15,
                              epatient_16_col = epatient_16,
                              eresponse_05_col = eresponse_05,
                              esituation_11_col = esituation_11,
                              esituation_12_col = esituation_12,
                              emedications_03_col = emedications_03
                         )
#> Running `seizure_02_population()`  [Working on 1 of 10 tasks] ●●●●─────────────…
#> Running `seizure_02_population()`  [Working on 2 of 10 tasks] ●●●●●●●──────────…
#> Running `seizure_02_population()`  [Working on 3 of 10 tasks] ●●●●●●●●●●───────…
#> Running `seizure_02_population()`  [Working on 4 of 10 tasks] ●●●●●●●●●●●●●────…
#> Running `seizure_02_population()`  [Working on 5 of 10 tasks] ●●●●●●●●●●●●●●●●─…
#> Running `seizure_02_population()`  [Working on 6 of 10 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `seizure_02_population()`  [Working on 7 of 10 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `seizure_02_population()`  [Working on 8 of 10 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `seizure_02_population()`  [Working on 9 of 10 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `seizure_02_population()`  [Working on 10 of 10 tasks] ●●●●●●●●●●●●●●●●…
#> 

# show the results of filtering at each step
result$filter_process
#> # A tibble: 7 × 2
#>   filter               count
#>   <chr>                <int>
#> 1 911 calls                5
#> 2 Seizure cases            5
#> 3 Benzodiazepine cases     5
#> 4 Adults denominator       2
#> 5 Peds denominator         3
#> 6 Initial population       5
#> 7 Total dataset            5
```
