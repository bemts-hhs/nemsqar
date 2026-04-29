# Airway-18 Populations

This function processes and analyzes the dataset to generate the
populations of interest needed to perform calculations to obtain
performance data.

## Usage

``` r
airway_18_population(
  df = NULL,
  patient_scene_table = NULL,
  procedures_table = NULL,
  vitals_table = NULL,
  airway_table = NULL,
  response_table = NULL,
  erecord_01_col,
  incident_date_col = NULL,
  patient_DOB_col = NULL,
  epatient_15_col,
  epatient_16_col,
  eresponse_05_col,
  eprocedures_01_col,
  eprocedures_02_col,
  eprocedures_03_col,
  eprocedures_06_col,
  eairway_02_col = NULL,
  eairway_04_col = NULL,
  evitals_01_col,
  evitals_16_col
)
```

## Arguments

- df:

  A dataframe or tibble contianing EMS data where each row represents an
  observation and columns represent features.

- patient_scene_table:

  A data.frame or tibble containing at least ePatient, and eScene as a
  fact table.

- procedures_table:

  A dataframe or tibble containing at least the eProcedures fields
  needed.

- vitals_table:

  A dataframe or tibble containing at least the eVitals fields needed.

- airway_table:

  A data frame or tibble containing only the eAirway fields needed for
  this measure's calculations. Default is `NULL`.

- response_table:

  A data.frame or tibble containing at least the eResponse fields needed
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

- eprocedures_01_col:

  Date-time or POSIXct column for procedures

- eprocedures_02_col:

  Column name for whether or not the procedure was performed prior to
  EMS care being provided.

- eprocedures_03_col:

  Column containing procedure codes with or without procedure names.

- eprocedures_06_col:

  Column indicating whether or not procedure was successful.

- eairway_02_col:

  Column name for date/time airway device placement confirmation.
  Default is `NULL`.

- eairway_04_col:

  Column name for confirmation of airway device placement. Default is
  `NULL`.

- evitals_01_col:

  Date-time or POSIXct column containing vital signs date/time

- evitals_16_col:

  Column with numeric value of the patient's exhaled end tidal carbon
  dioxide (ETCO2) level measured as a unit of pressure in millimeters of
  mercury (mmHg), percentage or, kilopascal (kPa).

## Value

A list that contains the following:

- a tibble with counts for each filtering step,

- a tibble for each population of interest

- a tibble for the initial population

- a tibble for the total dataset with computations

## Author

Nicolas Foss, Ed.D., MS, Samuel Kordik, BBA, BS

## Examples

``` r
# If you are sourcing your data from a SQL database connection
# or if you have your data in several different tables,
# you can pass table inputs versus a single data.frame or tibble

# create tables to test correct functioning

  # patient table
  patient_table <- tibble::tibble(

    erecord_01 = rep(c("R1", "R2", "R3", "R4", "R5"), 2),
    incident_date = rep(as.Date(c("2025-01-01", "2025-01-05", "2025-02-01",
    "2025-01-01", "2025-06-01")), 2),
    patient_dob = rep(as.Date(c("2000-01-01", "2020-01-01", "2023-02-01",
                                "2023-01-01", "1970-06-01")), 2),
    epatient_15 = rep(c(25, 5, 2, 2, 55), 2),  # Ages
    epatient_16 = rep(c("Years", "Years", "Years", "Years", "Years"), 2)

  )

  # response table
  response_table <- tibble::tibble(

    erecord_01 = rep(c("R1", "R2", "R3", "R4", "R5"), 2),
    eresponse_05 = rep(2205001, 10)

  )

  # vitals table
  vitals_table <- tibble::tibble(

    erecord_01 = rep(c("R1", "R2", "R3", "R4", "R5"), 2),
    evitals_01 = lubridate::as_datetime(c("2025-01-01 23:02:00",
    "2025-01-05 12:03:00", "2025-02-01 19:04:00", "2025-01-01 05:05:00",
    "2025-06-01 13:01:00", "2025-01-01 23:02:00",
    "2025-01-05 12:03:00", "2025-02-01 19:04:00", "2025-01-01 05:05:00",
    "2025-06-01 13:06:00")),
    evitals_16 = rep(c(5, 6, 7, 8, 9), 2)

  )

  # airway table
  airway_table <- tibble::tibble(
  erecord_01 = rep(c("R1", "R2", "R3", "R4", "R5"), 2),
  eairway_02 = rep(lubridate::as_datetime(c("2025-01-01 23:05:00",
    "2025-01-05 12:02:00", "2025-02-01 19:03:00", "2025-01-01 05:04:00",
    "2025-06-01 13:06:00")), 2),
  eairway_04 = rep(4004019, 10)
  )

  # procedures table
  procedures_table <- tibble::tibble(

    erecord_01 = rep(c("R1", "R2", "R3", "R4", "R5"), 2),
    eprocedures_01 = rep(lubridate::as_datetime(c("2025-01-01 23:00:00",
    "2025-01-05 12:00:00", "2025-02-01 19:00:00", "2025-01-01 05:00:00",
    "2025-06-01 13:00:00")), 2),
    eprocedures_02 = rep("No", 10),
    eprocedures_03 = rep(c(16883004, 112798008, 78121007, 49077009,
                           673005), 2),
    eprocedures_06 = rep(9923003, 10)

  )

# Run the function
result <- airway_18_population(df = NULL,
         patient_scene_table = patient_table,
         procedures_table = procedures_table,
         vitals_table = vitals_table,
         response_table = response_table,
         airway_table = airway_table,
         erecord_01_col = erecord_01,
         incident_date_col = incident_date,
         patient_DOB_col = patient_dob,
         epatient_15_col = epatient_15,
         epatient_16_col = epatient_16,
         eresponse_05_col = eresponse_05,
         eprocedures_01_col = eprocedures_01,
         eprocedures_02_col = eprocedures_02,
         eprocedures_03_col = eprocedures_03,
         eprocedures_06_col = eprocedures_06,
         evitals_01_col = evitals_01,
         evitals_16_col = evitals_16,
         eairway_02_col = eairway_02,
         eairway_04_col = eairway_04
         )
#> Running `airway_18_population()`  [Working on 1 of 13 tasks] ●●●───────────────…
#> Running `airway_18_population()`  [Working on 2 of 13 tasks] ●●●●●●────────────…
#> Running `airway_18_population()`  [Working on 3 of 13 tasks] ●●●●●●●●──────────…
#> Running `airway_18_population()`  [Working on 4 of 13 tasks] ●●●●●●●●●●────────…
#> Running `airway_18_population()`  [Working on 5 of 13 tasks] ●●●●●●●●●●●●●─────…
#> Running `airway_18_population()`  [Working on 6 of 13 tasks] ●●●●●●●●●●●●●●●───…
#> Running `airway_18_population()`  [Working on 7 of 13 tasks] ●●●●●●●●●●●●●●●●●─…
#> Running `airway_18_population()`  [Working on 8 of 13 tasks] ●●●●●●●●●●●●●●●●●●…
#> Running `airway_18_population()`  [Working on 9 of 13 tasks] ●●●●●●●●●●●●●●●●●●…
#> Running `airway_18_population()`  [Working on 10 of 13 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `airway_18_population()`  [Working on 11 of 13 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `airway_18_population()`  [Working on 12 of 13 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `airway_18_population()`  [Working on 13 of 13 tasks] ●●●●●●●●●●●●●●●●●…
#> 

# show the results of filtering at each step
result$filter_process
#> # A tibble: 12 × 2
#>    filter                                                                  count
#>    <chr>                                                                   <dbl>
#>  1 Invasive airway procedures                                                  5
#>  2 Successful invasive airway procedures                                       5
#>  3 911 calls                                                                   5
#>  4 Successful invasive airway procedures performed after EMS arrival           5
#>  5 Waveform ETCO2 used                                                         5
#>  6 Airway device placement confirmed after airway procedure                    5
#>  7 Vitals taken after airway procedure where waveform ETCO2 >= 5               5
#>  8 Waveform ETCO2 >= 5                                                         6
#>  9 Successful invasive airway procedures with waveform ETCO2 confirmed po…     5
#> 10 Adults denominator                                                          2
#> 11 Peds denominator                                                            3
#> 12 Total procedures in dataset                                                 5
```
