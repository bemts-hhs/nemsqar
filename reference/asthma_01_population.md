# Asthma-01 Populations

Filters data down to the target populations for Asthma-01, and
categorizes records to identify needed information for the calculations.

Identifies key categories related to asthma-related incidents in an EMS
dataset, specifically focusing on cases where 911 was called for
respiratory distress, and certain medications were administered. This
function segments the data by age into adult and pediatric populations,
computing the proportion of cases that received beta-agonist treatment.

## Usage

``` r
asthma_01_population(
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
# If you are sourcing your data from a SQL database connection
# or if you have your data in several different tables,
# you can pass table inputs versus a single data.frame or tibble

# create tables to test correct functioning

# patient table
patient_table <- tibble::tibble(

  erecord_01 = 1:3,
  incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01")),
  patient_dob = as.Date(c("2000-01-01", "2020-01-01", "2023-01-01")),
  epatient_15 = c(25, 5, 2),
  epatient_16 = c("years", "years", "months")

)

# response table
response_table <- tibble::tibble(

  erecord_01 = 1:3,
  eresponse_05 = c("2205001", "2205009", "2205003")

)

# situation table
situation_table <- tibble::tibble(

  erecord_01 = 1:3,
  esituation_11 = c("weakness", "asthma", "bronchospasm"),
  esituation_12 = c("asthma", "weakness", "weakness")
)

# medications table
medications_table <- tibble::tibble(

  erecord_01 = 1:3,
  emedications_03 = c("albuterol", "levalbuterol", "metaproterenol")

)

# test the success of the function
result <- asthma_01_population(patient_scene_table = patient_table,
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
#> Running `asthma_01_population()`  [Working on 1 of 10 tasks] ●●●●──────────────…
#> Running `asthma_01_population()`  [Working on 2 of 10 tasks] ●●●●●●●───────────…
#> Running `asthma_01_population()`  [Working on 3 of 10 tasks] ●●●●●●●●●●────────…
#> Running `asthma_01_population()`  [Working on 4 of 10 tasks] ●●●●●●●●●●●●●─────…
#> Running `asthma_01_population()`  [Working on 5 of 10 tasks] ●●●●●●●●●●●●●●●●──…
#> Running `asthma_01_population()`  [Working on 6 of 10 tasks] ●●●●●●●●●●●●●●●●●●…
#> Running `asthma_01_population()`  [Working on 7 of 10 tasks] ●●●●●●●●●●●●●●●●●●…
#> Running `asthma_01_population()`  [Working on 8 of 10 tasks] ●●●●●●●●●●●●●●●●●●…
#> Running `asthma_01_population()`  [Working on 9 of 10 tasks] ●●●●●●●●●●●●●●●●●●…
#> Running `asthma_01_population()`  [Working on 10 of 10 tasks] ●●●●●●●●●●●●●●●●●…
#> 

# show the results of filtering at each step
result$filter_process
#> # A tibble: 7 × 2
#>   filter             count
#>   <chr>              <int>
#> 1 911 calls              3
#> 2 Asthma cases           2
#> 3 Beta agonist cases     3
#> 4 Adults denominator     1
#> 5 Peds denominator       1
#> 6 Initial population     2
#> 7 Total dataset          3
```
