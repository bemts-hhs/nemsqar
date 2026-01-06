# Safety-01 Populations

Filters data down to the target populations for Safety-01, and
categorizes records to identify needed information for the calculations.

Identifies key categories related to 911 responses where "lights and
sirens" were not used in an EMS dataset. This function segments the data
by age into adult and pediatric populations.

## Usage

``` r
safety_01_population(
  df = NULL,
  patient_scene_table = NULL,
  response_table = NULL,
  erecord_01_col,
  incident_date_col = NULL,
  patient_DOB_col = NULL,
  epatient_15_col,
  epatient_16_col,
  eresponse_05_col,
  eresponse_24_col
)
```

## Arguments

- df:

  A data frame or tibble containing EMS data.

- patient_scene_table:

  A data frame or tibble containing only epatient and escene fields as a
  fact table. Default is `NULL`.

- response_table:

  A data frame or tibble containing only the eresponse fields needed for
  this measure's calculations. Default is `NULL`.

- erecord_01_col:

  Column name containing the unique patient record identifier.

- incident_date_col:

  Date or POSIXct column indicating the date of the incident.

- patient_DOB_col:

  Date or POSIXct column for the patient’s date of birth

- epatient_15_col:

  Column containing age.

- epatient_16_col:

  Column for age units.

- eresponse_05_col:

  Column containing response mode codes (e.g., 911 response codes).

- eresponse_24_col:

  Column detailing additional response descriptors as text.

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
    eresponse_05 = rep(2205001, 5),
    eresponse_24 = rep("No Lights or Sirens", 5)

  )

# Run the function
result <- safety_01_population(patient_scene_table = patient_table,
                              response_table = response_table,
                              erecord_01_col = erecord_01,
                              incident_date_col = incident_date,
                              patient_DOB_col = patient_dob,
                              epatient_15_col = epatient_15,
                              epatient_16_col = epatient_16,
                              eresponse_05_col = eresponse_05,
                              eresponse_24_col = eresponse_24
                        )
#> Running `safety_01_population()`  [Working on 1 of 9 tasks] ●●●●───────────────…
#> Running `safety_01_population()`  [Working on 2 of 9 tasks] ●●●●●●●●───────────…
#> Running `safety_01_population()`  [Working on 3 of 9 tasks] ●●●●●●●●●●●────────…
#> Running `safety_01_population()`  [Working on 4 of 9 tasks] ●●●●●●●●●●●●●●─────…
#> Running `safety_01_population()`  [Working on 5 of 9 tasks] ●●●●●●●●●●●●●●●●●●─…
#> Running `safety_01_population()`  [Working on 6 of 9 tasks] ●●●●●●●●●●●●●●●●●●●…
#> Running `safety_01_population()`  [Working on 7 of 9 tasks] ●●●●●●●●●●●●●●●●●●●…
#> Running `safety_01_population()`  [Working on 8 of 9 tasks] ●●●●●●●●●●●●●●●●●●●…
#> Running `safety_01_population()`  [Working on 9 of 9 tasks] ●●●●●●●●●●●●●●●●●●●…
#> 

# show the results of filtering at each step
result$filter_process
#> # A tibble: 6 × 2
#>   filter               count
#>   <chr>                <int>
#> 1 911 calls                5
#> 2 No lights and sirens     5
#> 3 Adults denominator       2
#> 4 Peds denominator         3
#> 5 Initial population       5
#> 6 Total dataset            5
```
