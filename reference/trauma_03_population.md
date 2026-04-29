# Trauma-03 Populations

Filters data down to the target populations for Trauma-08, and
categorizes records to identify needed information for the calculations.

Identifies key categories to records that are 911 request for patients
whose pain score was lowered during the EMS encounter. based on specific
criteria and calculates related ECG measures. This function segments the
data by age into adult and pediatric populations.

## Usage

``` r
trauma_03_population(
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
  evitals_01_col,
  evitals_27_col = NULL,
  evitals_27_initial_col = NULL,
  evitals_27_last_col = NULL,
  edisposition_28_col,
  transport_disposition_col
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

- evitals_01_col:

  The column for the date/time vital signs were taken on the patient.

- evitals_27_col:

  Column giving the patient's indication of pain from a scale of 0-10.

- evitals_27_initial_col:

  The column for the initial pain scale score. Default is `NULL`.

- evitals_27_last_col:

  The column for the last pain scale score. Default is `NULL`.

- edisposition_28_col:

  Column name for patient care disposition details.

- transport_disposition_col:

  One or more unquoted column names (such as edisposition.12,
  edisposition.30) containing transport disposition for an EMS event
  identifying whether a transport occurred and by which unit.

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

  # vitals table for a single pain scale column
  vitals_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    evitals_01 = lubridate::as_datetime(c("2025-01-01 12:00:00", "2025-01-05
    18:00:00", "2025-02-01 06:00:00", "2025-01-01 01:00:00", "2025-06-01
    14:00:00"))
  ) |>
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

  # disposition table
  disposition_table <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    edisposition_28 = rep(4228001, 5),
    edisposition_30 = c(4230001, 4230003, 4230001, 4230007, 4230007)
  )

# test the success of the function
# use the single pain scale column
  result <- trauma_03_population(patient_scene_table = patient_table,
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
                        evitals_01_col = evitals_01,
                        evitals_27_initial_col = NULL,
                        evitals_27_last_col = NULL,
                        evitals_27_col = evitals_27,
                        edisposition_28_col = edisposition_28,
                        transport_disposition_col = edisposition_30
                        )
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

# show the results of filtering at each step
result$filter_process
#> # A tibble: 10 × 2
#>    filter                                                       count
#>    <chr>                                                        <int>
#>  1 911 calls                                                        5
#>  2 Non-missing vital sign date-time with initial pain score > 0     4
#>  3 Transports                                                       5
#>  4 Injury cases                                                     5
#>  5 Patient evaluated and care provided                              5
#>  6 Pain scale decreased                                             4
#>  7 Adults denominator                                               1
#>  8 Peds denominator                                                 3
#>  9 Initial population                                               4
#> 10 Total dataset                                                    5
```
