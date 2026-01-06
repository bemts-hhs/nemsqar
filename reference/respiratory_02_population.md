# Respiratory-02 Populations

The `respiratory_02_population` function calculates metrics for
pediatric and adult respiratory populations based on pre-defined
criteria, such as low oxygen saturation and specific medication or
procedure codes. It returns a summary table of the overall, pediatric,
and adult populations, showing counts and proportions.

## Usage

``` r
respiratory_02_population(
  df = NULL,
  patient_scene_table = NULL,
  response_table = NULL,
  vitals_table = NULL,
  medications_table = NULL,
  procedures_table = NULL,
  erecord_01_col,
  incident_date_col = NULL,
  patient_DOB_col = NULL,
  epatient_15_col,
  epatient_16_col,
  eresponse_05_col,
  evitals_12_col,
  emedications_03_col,
  eprocedures_03_col
)
```

## Arguments

- df:

  A data frame containing incident data with each row representing an
  observation.

- patient_scene_table:

  A data.frame or tibble containing at least epatient and escene fields
  as a fact table.

- response_table:

  A data.frame or tibble containing at least the eresponse fields needed
  for this measure's calculations.

- vitals_table:

  A data.frame or tibble containing at least the evitals fields needed
  for this measure's calculations.

- medications_table:

  A data.frame or tibble containing only the emedications fields needed
  for this measure's calculations.

- procedures_table:

  A data.frame or tibble containing only the eprocedures fields needed
  for this measure's calculations.

- erecord_01_col:

  Column name for eRecord.01, used to form a unique patient ID.

- incident_date_col:

  Column that contains the incident date. This defaults to `NULL` as it
  is optional in case not available due to PII restrictions.

- patient_DOB_col:

  Column that contains the patient's date of birth. This defaults to
  `NULL` as it is optional in case not available due to PII
  restrictions.

- epatient_15_col:

  integer Column giving the calculated age value.

- epatient_16_col:

  Column giving the provided age unit value.

- eresponse_05_col:

  Column name for response codes (e.g., incident type).

- evitals_12_col:

  Column name for oxygen saturation (SpO2) values.

- emedications_03_col:

  Column name for medication codes.

- eprocedures_03_col:

  Column name for procedure codes.

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

  # medications table
  medications_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    emedications_03 = c("Oxygen", "Oxygen", "Oxygen", "Oxygen", "Oxygen")

  )

  # vitals table
  vitals_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    evitals_12 = c(60, 59, 58, 57, 56),

  )

  # procedures table
  procedures_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    eprocedures_03 = rep("applicable thing", 5)

  )

# Run the function
result <- respiratory_02_population(patient_scene_table = patient_table,
                              response_table = response_table,
                              medications_table = medications_table,
                              vitals_table = vitals_table,
                              procedures_table = procedures_table,
                              erecord_01_col = erecord_01,
                              incident_date_col = incident_date,
                              patient_DOB_col = patient_dob,
                              epatient_15_col = epatient_15,
                              epatient_16_col = epatient_16,
                              eresponse_05_col = eresponse_05,
                              emedications_03_col = emedications_03,
                              evitals_12_col = evitals_12,
                              eprocedures_03_col = eprocedures_03
                             )
#> Running `respiratory_02_population()`  [Completed 1 of 11 tasks] ●●●●──────────…
#> Running `respiratory_02_population()`  [Completed 2 of 11 tasks] ●●●●●●────────…
#> Running `respiratory_02_population()`  [Completed 3 of 11 tasks] ●●●●●●●●●─────…
#> Running `respiratory_02_population()`  [Completed 4 of 11 tasks] ●●●●●●●●●●●●──…
#> Running `respiratory_02_population()`  [Completed 5 of 11 tasks] ●●●●●●●●●●●●●●…
#> Running `respiratory_02_population()`  [Completed 6 of 11 tasks] ●●●●●●●●●●●●●●…
#> Running `respiratory_02_population()`  [Completed 7 of 11 tasks] ●●●●●●●●●●●●●●…
#> Running `respiratory_02_population()`  [Completed 8 of 11 tasks] ●●●●●●●●●●●●●●…
#> Running `respiratory_02_population()`  [Completed 9 of 11 tasks] ●●●●●●●●●●●●●●…
#> Running `respiratory_02_population()`  [Completed 10 of 11 tasks] ●●●●●●●●●●●●●…
#> Running `respiratory_02_population()`  [Completed 11 of 11 tasks] ●●●●●●●●●●●●●…
#> 

# show the results of filtering at each step
result$filter_process
#> # A tibble: 8 × 2
#>   filter                   count
#>   <chr>                    <int>
#> 1 Oxygen given as med          5
#> 2 Oxygen therapy procedure     0
#> 3 Pulse oximetry < 90          5
#> 4 911 calls                    5
#> 5 Adults denominator           2
#> 6 Peds denominator             3
#> 7 Initial population           5
#> 8 Total dataset                5
```
