# Trauma-01 Population

Filters data down to the target populations for Trauma-08, and
categorizes records to identify needed information for the calculations.

Identifies key categories to records that are 911 requests for patients
with injury who were assessed for pain based on specific criteria and
calculates related ECG measures. This function segments the data by age
into adult and pediatric populations.

## Usage

``` r
trauma_01_population(
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
  evitals_23_col,
  evitals_26_col,
  evitals_27_col,
  edisposition_28_col,
  transport_disposition_col
)
```

## Arguments

- df:

  A data frame or tibble containing EMS records. Default is `NULL`.

- patient_scene_table:

  A data frame or tibble containing only epatient and escene fields as a
  fact table. Default is `NULL`.

- response_table:

  A data frame or tibble containing only the eresponse fields needed for
  this measure's calculations. Default is `NULL`.

- situation_table:

  A data.frame or tibble containing only the esituation fields needed
  for this measure's calculations. Default is `NULL`.

- disposition_table:

  A data.frame or tibble containing only the edisposition fields needed
  for this measure's calculations. Default is `NULL`.

- vitals_table:

  A data.frame or tibble containing only the evitals fields needed for
  this measure's calculations. Default is `NULL`.

- erecord_01_col:

  Column name representing the EMS record ID.

- incident_date_col:

  Column name for the incident date. Default is `NULL`.

- patient_DOB_col:

  Column name for the patient's date of birth. Default is `NULL`.

- epatient_15_col:

  Column name for the patient's age in numeric format.

- epatient_16_col:

  Column name for the unit of age (e.g., "Years", "Months").

- esituation_02_col:

  Column name indicating if the situation involved an injury.

- eresponse_05_col:

  Column name for the type of EMS response (e.g., 911 call).

- evitals_23_col:

  Column name for the Glasgow Coma Scale (GCS) total score.

- evitals_26_col:

  Column name for AVPU (Alert, Voice, Pain, Unresponsive) status.

- evitals_27_col:

  Column name for the pain scale assessment.

- edisposition_28_col:

  Column name for patient care disposition details.

- transport_disposition_col:

  Column name for transport disposition details.

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
    esituation_02 = rep("Yes", 5),
  )

  # vitals table
  vitals_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    evitals_23 = rep(15, 5),
    evitals_26 = rep("Alert", 5),
    evitals_27 = c(0, 2, 4, 6, 8)
  )

  # disposition table
  disposition_table <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    edisposition_28 = rep(4228001, 5),
    edisposition_30 = c(4230001, 4230003, 4230001, 4230007, 4230007)
  )

  # test the success of the function
  result <- trauma_01_population(patient_scene_table = patient_table,
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
                     evitals_23_col = evitals_23,
                     evitals_26_col = evitals_26,
                     evitals_27_col = evitals_27,
                     edisposition_28_col = edisposition_28,
                     transport_disposition_col = edisposition_30
                     )
#> Running `trauma_01_population()`  [Working on 1 of 14 tasks] ●●●───────────────…
#> Running `trauma_01_population()`  [Working on 2 of 14 tasks] ●●●●●─────────────…
#> Running `trauma_01_population()`  [Working on 3 of 14 tasks] ●●●●●●●───────────…
#> Running `trauma_01_population()`  [Working on 4 of 14 tasks] ●●●●●●●●●●────────…
#> Running `trauma_01_population()`  [Working on 5 of 14 tasks] ●●●●●●●●●●●●──────…
#> Running `trauma_01_population()`  [Working on 6 of 14 tasks] ●●●●●●●●●●●●●●────…
#> Running `trauma_01_population()`  [Working on 7 of 14 tasks] ●●●●●●●●●●●●●●●●──…
#> Running `trauma_01_population()`  [Working on 8 of 14 tasks] ●●●●●●●●●●●●●●●●●●…
#> Running `trauma_01_population()`  [Working on 9 of 14 tasks] ●●●●●●●●●●●●●●●●●●…
#> Running `trauma_01_population()`  [Working on 10 of 14 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_01_population()`  [Working on 11 of 14 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_01_population()`  [Working on 12 of 14 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_01_population()`  [Working on 13 of 14 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `trauma_01_population()`  [Working on 14 of 14 tasks] ●●●●●●●●●●●●●●●●●…
#> 

# show the results of filtering at each step
result$filter_process
#> # A tibble: 11 × 2
#>    filter                              count
#>    <chr>                               <int>
#>  1 911 calls                               5
#>  2 GCS is 15                               5
#>  3 AVPU is alert                           5
#>  4 Transports                              5
#>  5 Injury cases                            5
#>  6 Patient evaluated and care provided     5
#>  7 Pain scale administered                 5
#>  8 Adults denominator                      2
#>  9 Peds denominator                        3
#> 10 Initial population                      5
#> 11 Total dataset                           5
```
