# Respiratory-01 Populations

The `respiratory_01_population` function filters and analyzes data
related to emergency 911 respiratory distress incidents, providing the
adult, pediatric, and initial populations. This function uses specific
data columns for 911 response codes, primary and secondary impressions,
and vital signs to filter a dataset down to the populations of interest.

## Usage

``` r
respiratory_01_population(
  df = NULL,
  patient_scene_table = NULL,
  response_table = NULL,
  situation_table = NULL,
  vitals_table = NULL,
  erecord_01_col,
  incident_date_col = NULL,
  patient_DOB_col = NULL,
  epatient_15_col,
  epatient_16_col,
  eresponse_05_col,
  esituation_11_col,
  esituation_12_col,
  evitals_12_col,
  evitals_14_col
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

- evitals_12_col:

  Numeric column containing pulse oximetry values.

- evitals_14_col:

  Column containing data on patient's respiratory rate expressed as a
  number per minute.

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
  esituation_11 = c(rep("J80", 3), rep("I50.9", 2)),
  esituation_12 = c(rep("J80", 2), rep("I50.9", 3))
)

# vitals table
vitals_table <- tibble::tibble(

  erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
  evitals_12 = c(60, 59, 58, 57, 56),
  evitals_14 = c(16, 15, 14, 13, 12)

)

# Run the function
result <- respiratory_01_population(patient_scene_table = patient_table,
                              response_table = response_table,
                              situation_table = situation_table,
                              vitals_table = vitals_table,
                              erecord_01_col = erecord_01,
                              incident_date_col = incident_date,
                              patient_DOB_col = patient_dob,
                              epatient_15_col = epatient_15,
                              epatient_16_col = epatient_16,
                              eresponse_05_col = eresponse_05,
                              esituation_11_col = esituation_11,
                              esituation_12_col = esituation_12,
                              evitals_12_col = evitals_12,
                              evitals_14_col = evitals_14
                             )
#> Running `respiratory_01_population()`  [Working on 1 of 13 tasks] ●●●──────────…
#> Running `respiratory_01_population()`  [Working on 2 of 13 tasks] ●●●●●●───────…
#> Running `respiratory_01_population()`  [Working on 3 of 13 tasks] ●●●●●●●●─────…
#> Running `respiratory_01_population()`  [Working on 4 of 13 tasks] ●●●●●●●●●●───…
#> Running `respiratory_01_population()`  [Working on 5 of 13 tasks] ●●●●●●●●●●●●●…
#> Running `respiratory_01_population()`  [Working on 6 of 13 tasks] ●●●●●●●●●●●●●…
#> Running `respiratory_01_population()`  [Working on 7 of 13 tasks] ●●●●●●●●●●●●●…
#> Running `respiratory_01_population()`  [Working on 8 of 13 tasks] ●●●●●●●●●●●●●…
#> Running `respiratory_01_population()`  [Working on 9 of 13 tasks] ●●●●●●●●●●●●●…
#> Running `respiratory_01_population()`  [Working on 10 of 13 tasks] ●●●●●●●●●●●●…
#> Running `respiratory_01_population()`  [Working on 12 of 13 tasks] ●●●●●●●●●●●●…
#> Running `respiratory_01_population()`  [Working on 13 of 13 tasks] ●●●●●●●●●●●●…
#> 

# show the results of filtering at each step
result$filter_process
#> # A tibble: 7 × 2
#>   filter                                    count
#>   <chr>                                     <int>
#> 1 Respiratory Distress                          5
#> 2 Pulse Oximetry and Respiratory Rate taken     5
#> 3 911 calls                                     5
#> 4 Adults denominator                            2
#> 5 Peds denominator                              3
#> 6 Initial population                            5
#> 7 Total dataset                                 5
```
