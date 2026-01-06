# Safety-04 Populations

Filters data down to the target populations for Safety-04, and
categorizes records to identify needed information for the calculations.

Identifies key categories related to a 911 request or interfacility
request for patients less than 8 years of age during which patients are
transported using a pediatric restraint device. This function segments
the data by age into adult and pediatric populations.

## Usage

``` r
safety_04_population(
  df = NULL,
  patient_scene_table = NULL,
  response_table = NULL,
  arrest_table = NULL,
  injury_table = NULL,
  procedures_table = NULL,
  disposition_table = NULL,
  erecord_01_col,
  incident_date_col = NULL,
  patient_DOB_col = NULL,
  epatient_15_col,
  epatient_16_col,
  eresponse_05_col,
  earrest_01_col,
  einjury_03_col,
  eprocedures_03_col,
  edisposition_14_col,
  transport_disposition_col
)
```

## Arguments

- df:

  A data frame or tibble containing EMS data where each row represents
  an individual observation.

- patient_scene_table:

  A data frame or tibble containing fields from epatient and escene
  needed for this measure's calculations.

- response_table:

  A data frame or tibble containing fields from eresponse needed for
  this measure's calculations.

- arrest_table:

  A data frame or tibble containing fields from earrest needed for this
  measure's calculations.

- injury_table:

  A data frame or tibble containing fields from einjury needed for this
  measure's calculations.

- procedures_table:

  A data frame or tibble containing fields from eprocedures needed for
  this measure's calculations.

- disposition_table:

  A data frame or tibble containing fields from edisposition needed for
  this measure's calculations.

- erecord_01_col:

  The column containing unique record identifiers for each encounter.

- incident_date_col:

  Column that contains the incident date. This defaults to `NULL` as it
  is optional in case not available due to PII restrictions.

- patient_DOB_col:

  Column that contains the patient's date of birth. This defaults to
  `NULL` as it is optional in case not available due to PII
  restrictions.

- epatient_15_col:

  Column name indicating the patient age.

- epatient_16_col:

  Column name for the unit of age (e.g., "Years," "Months").

- eresponse_05_col:

  Column containing response transport codes.

- earrest_01_col:

  Column with cardiac arrest status information.

- einjury_03_col:

  Column describing traumatic injuries, expected as a list or
  text-separated entries.

- eprocedures_03_col:

  Column listing procedures, assumed to contain multiple procedure
  codes/texts in each cell.

- edisposition_14_col:

  Column for transport dispositions.

- transport_disposition_col:

  Columns for primary and secondary transport dispositions.

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

  # disposition table
  disposition_table <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    edisposition_14 = rep(4214001, 5),
    edisposition_30 = rep(4230001, 5),
  )

  # arrest table
  arrest_table <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    earrest_01 = rep("No", 5)
  )

  # injury table
  injury_table <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    einjury_03 = rep("non-injury", 5)
  )

  # procedures table
  procedures_table <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    eprocedures_03 = rep("other response", 5)
  )

  # test the success of the function
  result <- safety_04_population(patient_scene_table = patient_table,
                        response_table = response_table,
                        arrest_table = arrest_table,
                        injury_table = injury_table,
                        procedures_table = procedures_table,
                        disposition_table = disposition_table,
                        erecord_01_col = erecord_01,
                        incident_date_col = incident_date,
                        patient_DOB_col = patient_dob,
                        epatient_15_col = epatient_15,
                        epatient_16_col = epatient_16,
                        eresponse_05_col = eresponse_05,
                        earrest_01_col = earrest_01,
                        einjury_03_col = einjury_03,
                        edisposition_14_col = edisposition_14,
                        transport_disposition_col = edisposition_30,
                        eprocedures_03_col = eprocedures_03
                        )
#> Running `safety_04_population()`  [Working on 1 of 13 tasks] ●●●───────────────…
#> Running `safety_04_population()`  [Working on 2 of 13 tasks] ●●●●●●────────────…
#> Running `safety_04_population()`  [Working on 3 of 13 tasks] ●●●●●●●●──────────…
#> Running `safety_04_population()`  [Working on 4 of 13 tasks] ●●●●●●●●●●────────…
#> Running `safety_04_population()`  [Working on 5 of 13 tasks] ●●●●●●●●●●●●●─────…
#> Running `safety_04_population()`  [Working on 6 of 13 tasks] ●●●●●●●●●●●●●●●───…
#> Running `safety_04_population()`  [Working on 7 of 13 tasks] ●●●●●●●●●●●●●●●●●─…
#> Running `safety_04_population()`  [Working on 8 of 13 tasks] ●●●●●●●●●●●●●●●●●●…
#> Running `safety_04_population()`  [Working on 9 of 13 tasks] ●●●●●●●●●●●●●●●●●●…
#> Running `safety_04_population()`  [Working on 10 of 13 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `safety_04_population()`  [Working on 11 of 13 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `safety_04_population()`  [Working on 12 of 13 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `safety_04_population()`  [Working on 13 of 13 tasks] ●●●●●●●●●●●●●●●●●…
#> 

# show the results of filtering at each step
result$filter_process
#> # A tibble: 10 × 2
#>    filter                              count
#>    <chr>                               <int>
#>  1 Transport runs                          5
#>  2 Interfacility runs                      0
#>  3 Cardiac arrest calls                    0
#>  4 Severe injury calls                     0
#>  5 Calls involving long board              0
#>  6 Calls involving an airway procedure     0
#>  7 Car seat used                           5
#>  8 Peds denominator                        3
#>  9 Initial population                      3
#> 10 Total dataset                           5
```
