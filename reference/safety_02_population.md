# Safety-02 Populations

Filters data down to the target populations for Safety-02, and
categorizes records to identify needed information for the calculations.

Identifies key categories related to a 911 request during which lights
and sirens were not used during patient transport. This function
segments the data by age into adult and pediatric populations.

## Usage

``` r
safety_02_population(
  df = NULL,
  patient_scene_table = NULL,
  response_table = NULL,
  disposition_table = NULL,
  erecord_01_col,
  incident_date_col = NULL,
  patient_DOB_col = NULL,
  epatient_15_col,
  epatient_16_col,
  eresponse_05_col,
  edisposition_18_col,
  edisposition_28_col,
  transport_disposition_col,
  transport_disposition_cols = lifecycle::deprecated()
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

- disposition_table:

  A data.frame or tibble containing only the edisposition fields needed
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

- edisposition_18_col:

  Column giving documentation of transport mode techniques for this EMS
  response.

- edisposition_28_col:

  Column giving patient disposition for an EMS event identifying whether
  a patient was evaluated and care or services were provided.

- transport_disposition_col:

  One or more unquoted column names (such as edisposition.12,
  edisposition.30) containing transport disposition for an EMS event
  identifying whether a transport occurred and by which unit.

- transport_disposition_cols:

  **\[deprecated\]** Use `transport_disposition_col` instead.

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
    edisposition_18 = rep(4218015, 5),
    edisposition_28 = rep(4228001, 5),
    edisposition_30 = rep(4230001, 5)
  )

  # test the success of the function
  result <- safety_02_population(patient_scene_table = patient_table,
                        response_table = response_table,
                        disposition_table = disposition_table,
                        erecord_01_col = erecord_01,
                        incident_date_col = incident_date,
                        patient_DOB_col = patient_dob,
                        epatient_15_col = epatient_15,
                        epatient_16_col = epatient_16,
                        eresponse_05_col = eresponse_05,
                        edisposition_18_col = edisposition_18,
                        edisposition_28_col = edisposition_28,
                        transport_disposition_col = edisposition_30
                        )
#> Running `safety_02_population()`  [Working on 1 of 11 tasks] ●●●●──────────────…
#> Running `safety_02_population()`  [Working on 2 of 11 tasks] ●●●●●●────────────…
#> Running `safety_02_population()`  [Working on 3 of 11 tasks] ●●●●●●●●●─────────…
#> Running `safety_02_population()`  [Working on 4 of 11 tasks] ●●●●●●●●●●●●──────…
#> Running `safety_02_population()`  [Working on 5 of 11 tasks] ●●●●●●●●●●●●●●●───…
#> Running `safety_02_population()`  [Working on 6 of 11 tasks] ●●●●●●●●●●●●●●●●●─…
#> Running `safety_02_population()`  [Working on 7 of 11 tasks] ●●●●●●●●●●●●●●●●●●…
#> Running `safety_02_population()`  [Working on 8 of 11 tasks] ●●●●●●●●●●●●●●●●●●…
#> Running `safety_02_population()`  [Working on 9 of 11 tasks] ●●●●●●●●●●●●●●●●●●…
#> Running `safety_02_population()`  [Working on 10 of 11 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `safety_02_population()`  [Working on 11 of 11 tasks] ●●●●●●●●●●●●●●●●●…
#> 

# show the results of filtering at each step
result$filter_process
#> # A tibble: 8 × 2
#>   filter                               count
#>   <chr>                                <int>
#> 1 911 calls                                5
#> 2 Patients evaluated and care provided     5
#> 3 Transport runs                           5
#> 4 No lights and sirens                     5
#> 5 Adults denominator                       2
#> 6 Peds denominator                         3
#> 7 Initial population                       5
#> 8 Total dataset                            5
```
