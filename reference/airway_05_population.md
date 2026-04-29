# Airway-05 Population

This function processes and analyzes the dataset to generate the
populations of interest needed to perform calculations to obtain
performance data.

## Usage

``` r
airway_05_population(
  df = NULL,
  patient_scene_table = NULL,
  response_table = NULL,
  arrest_table = NULL,
  procedures_table = NULL,
  vitals_table = NULL,
  erecord_01_col,
  incident_date_col = NULL,
  patient_DOB_col = NULL,
  epatient_15_col,
  epatient_16_col,
  earrest_01_col,
  eresponse_05_col,
  evitals_01_col,
  evitals_12_col,
  eprocedures_01_col,
  eprocedures_02_col,
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

- arrest_table:

  A data.frame or tibble containing at least the eArrest fields needed
  for this measure's calculations.

- procedures_table:

  A dataframe or tibble containing at least the eProcedures fields
  needed.

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

- earrest_01_col:

  Column representing whether or not the patient is in arrest.

- eresponse_05_col:

  Column that contains eResponse.05 or the response type.

- evitals_01_col:

  Date-time or POSIXct column containing vital signs date/time

- evitals_12_col:

  Numeric column containing pulse oximetry values.

- eprocedures_01_col:

  Date-time or POSIXct column for procedures

- eprocedures_02_col:

  Column name for whether or not the procedure was performed prior to
  EMS care being provided.

- eprocedures_03_col:

  Column containing procedure codes with or without procedure names.

## Value

A list that contains the following:

- a tibble with counts for each filtering step,

- a tibble for each population of interest

- a tibble for the initial population

- a tibble for the total dataset with computations

## Author

Samuel Kordik, BBA, BS, Nicolas Foss Ed.D., MS

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
    evitals_01 = lubridate::as_datetime(c("2025-01-01 22:59:00",
    "2025-01-05 11:58:00", "2025-02-01 18:57:00", "2025-01-01 04:58:00",
    "2025-06-01 12:57:00", "2025-01-01 23:05:00", "2025-01-05 12:04:00",
    "2025-02-01 19:03:00", "2025-01-01 05:02:00", "2025-06-01 13:01:00")),
    evitals_12 = rep(c(90, 91, 92, 93, 94), 2)

  )

# arrest table
  arrest_table <- tibble::tibble(

    erecord_01 = rep(c("R1", "R2", "R3", "R4", "R5"), 2),
    earrest_01 = rep("No", 10)
  )

  # procedures table
  procedures_table <- tibble::tibble(

    erecord_01 = rep(c("R1", "R2", "R3", "R4", "R5"), 2),
    eprocedures_01 = rep(lubridate::as_datetime(c("2025-01-01 23:00:00",
    "2025-01-05 12:00:00", "2025-02-01 19:00:00", "2025-01-01 05:00:00",
    "2025-06-01 13:00:00")), 2),
    eprocedures_02 = rep("No", 10),
    eprocedures_03 = rep(c(16883004, 112798008, 78121007, 49077009,
                           673005), 2)

  )

# Run the function
result <- airway_05_population(df = NULL,
         patient_scene_table = patient_table,
         procedures_table = procedures_table,
         vitals_table = vitals_table,
         arrest_table = arrest_table,
         response_table = response_table,
         erecord_01_col = erecord_01,
         incident_date_col = incident_date,
         patient_DOB_col = patient_dob,
         epatient_15_col = epatient_15,
         epatient_16_col = epatient_16,
         eresponse_05_col = eresponse_05,
         eprocedures_01_col = eprocedures_01,
         eprocedures_02_col = eprocedures_02,
         eprocedures_03_col = eprocedures_03,
         earrest_01_col = earrest_01,
         evitals_01_col = evitals_01,
         evitals_12_col = evitals_12
         )
#> Running `airway_05_population()`  [Working on 1 of 15 tasks] ●●●───────────────…
#> Running `airway_05_population()`  [Working on 2 of 15 tasks] ●●●●●─────────────…
#> Running `airway_05_population()`  [Working on 3 of 15 tasks] ●●●●●●●───────────…
#> Running `airway_05_population()`  [Working on 4 of 15 tasks] ●●●●●●●●●─────────…
#> Running `airway_05_population()`  [Working on 5 of 15 tasks] ●●●●●●●●●●●───────…
#> Running `airway_05_population()`  [Working on 6 of 15 tasks] ●●●●●●●●●●●●●─────…
#> Running `airway_05_population()`  [Working on 7 of 15 tasks] ●●●●●●●●●●●●●●●───…
#> Running `airway_05_population()`  [Working on 8 of 15 tasks] ●●●●●●●●●●●●●●●●●─…
#> Running `airway_05_population()`  [Working on 9 of 15 tasks] ●●●●●●●●●●●●●●●●●●…
#> Running `airway_05_population()`  [Working on 10 of 15 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `airway_05_population()`  [Working on 11 of 15 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `airway_05_population()`  [Working on 12 of 15 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `airway_05_population()`  [Working on 13 of 15 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `airway_05_population()`  [Working on 14 of 15 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `airway_05_population()`  [Working on 15 of 15 tasks] ●●●●●●●●●●●●●●●●●…
#> 

# show the results of filtering at each step
result$filter_process
#> # A tibble: 11 × 2
#>    filter                                                        count
#>    <chr>                                                         <dbl>
#>  1 Invasive airway procedures                                        5
#>  2 911 calls                                                         5
#>  3 Excluded cardiac arrests                                          0
#>  4 Excluded newborns                                                 0
#>  5 All initial population intubation with adequate oxygen levels     1
#>  6 Adults intubation with adequate oxygen levels                     1
#>  7 Peds intubation with adequate oxygen levels                       0
#>  8 Adults denominator                                                2
#>  9 Peds denominator                                                  3
#> 10 Initial Population                                                5
#> 11 Total procedures in dataset                                       5
```
