# Pediatrics-03B Populations

Filters data down to the target populations for Pediatrics-03B, and
categorizes records to identify needed information for the calculations.

Identifies key categories related to diabetes/hypoglycemia incidents in
an EMS dataset, specifically focusing on cases where 911 was called for
diabetes/hypoglycemia distress, certain medications were administered,
and a weight is taken. This function segments the data into pediatric
populations, computing the proportion of cases that have a documented
weight.

## Usage

``` r
pediatrics_03b_population(
  df = NULL,
  patient_scene_table = NULL,
  response_table = NULL,
  exam_table = NULL,
  medications_table = NULL,
  erecord_01_col,
  incident_date_col = NULL,
  patient_DOB_col = NULL,
  epatient_15_col,
  epatient_16_col,
  eresponse_05_col,
  eexam_01_col,
  eexam_02_col,
  emedications_03_col,
  emedications_04_col
)
```

## Arguments

- df:

  A data frame or tibble containing emergency response records. Default
  is `NULL`.

- patient_scene_table:

  A data.frame or tibble containing only ePatient and eScene fields as a
  fact table. Default is `NULL`.

- response_table:

  A data.frame or tibble containing only the eResponse fields needed for
  this measure's calculations. Default is `NULL`.

- exam_table:

  A data.frame or tibble containing only the eExam fields needed for
  this measure's calculations. Default is `NULL`.

- medications_table:

  A data.frame or tibble containing only the eMedications fields needed
  for this measure's calculations. Default is `NULL`.

- erecord_01_col:

  Column for unique EMS record identifiers.

- incident_date_col:

  Column that contains the incident date. This defaults to `NULL` as it
  is optional in case not available due to PII restrictions.

- patient_DOB_col:

  Column that contains the patient's date of birth. This defaults to
  `NULL` as it is optional in case not available due to PII
  restrictions.

- epatient_15_col:

  Column giving the calculated age value.

- epatient_16_col:

  Column giving the provided age unit value.

- eresponse_05_col:

  Column containing the EMS response codes.

- eexam_01_col:

  Column containing documented weight information.

- eexam_02_col:

  Another column for weight documentation, if applicable.

- emedications_03_col:

  Column indicating medication administration.

- emedications_04_col:

  Column listing medications administered.

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

response_table <- tibble::tibble(

  erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
  eresponse_05 = rep(2205001, 5)

)

exam_table <- tibble::tibble(

  erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
  eexam_01 = c(60, 59, 58, 57, 56),
  eexam_02 = c("Red", "Purple", "Grey", "Yellow", "Orange")
)

medications_table <- tibble::tibble(

  erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
  emedications_03 = rep("stuff", 5),
  emedications_04 = c("Inhalation", "pill", "liquid", "pill", "liquid"),

)

# test the success of the function

result <- pediatrics_03b_population(patient_scene_table = patient_table,
                           response_table = response_table,
                           exam_table = exam_table,
                           medications_table = medications_table,
                           erecord_01_col = erecord_01,
                           incident_date_col = incident_date,
                           patient_DOB_col = patient_dob,
                           epatient_15_col = epatient_15,
                           epatient_16_col = epatient_16,
                           eresponse_05_col = eresponse_05,
                           emedications_03_col = emedications_03,
                           emedications_04_col = emedications_04,
                           eexam_01_col = eexam_01,
                           eexam_02_col = eexam_02
                           )
#> Running `pediatrics_03b_population()`  [Working on 1 of 9 tasks] ●●●●──────────…
#> Running `pediatrics_03b_population()`  [Working on 2 of 9 tasks] ●●●●●●●●──────…
#> Running `pediatrics_03b_population()`  [Working on 3 of 9 tasks] ●●●●●●●●●●●───…
#> Running `pediatrics_03b_population()`  [Working on 4 of 9 tasks] ●●●●●●●●●●●●●●…
#> Running `pediatrics_03b_population()`  [Working on 5 of 9 tasks] ●●●●●●●●●●●●●●…
#> Running `pediatrics_03b_population()`  [Working on 6 of 9 tasks] ●●●●●●●●●●●●●●…
#> Running `pediatrics_03b_population()`  [Working on 7 of 9 tasks] ●●●●●●●●●●●●●●…
#> Running `pediatrics_03b_population()`  [Working on 8 of 9 tasks] ●●●●●●●●●●●●●●…
#> Running `pediatrics_03b_population()`  [Working on 9 of 9 tasks] ●●●●●●●●●●●●●●…
#> 

# show the results of filtering at each step
result$filter_process
#> # A tibble: 6 × 2
#>   filter                count
#>   <chr>                 <int>
#> 1 Meds not missing          5
#> 2 Non-Weight Based Meds     1
#> 3 Documented Weight         5
#> 4 911 calls                 5
#> 5 Peds denominator          3
#> 6 Total dataset             5

```
