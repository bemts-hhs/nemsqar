# Airway-01 Calculation

Calculates the NEMSQA Airway-01 measure.

Calculates the proportion of times when the first endotracheal
intubation attempt is successful with no peri-intubation hypoxia or
hypotension.

## Usage

``` r
airway_01(
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
  evitals_06_col,
  evitals_12_col,
  eprocedures_01_col,
  eprocedures_02_col,
  eprocedures_03_col,
  eprocedures_05_col,
  eprocedures_06_col,
  confidence_interval = FALSE,
  method = c("wilson", "clopper-pearson"),
  conf.level = 0.95,
  correct = TRUE,
  ...
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

- evitals_06_col:

  Numeric column containing systolic blood pressure values.

- evitals_12_col:

  Numeric column containing pulse oximetry values.

- eprocedures_01_col:

  Date-time or POSIXct column for procedures

- eprocedures_02_col:

  Column name for whether or not the procedure was performed prior to
  EMS care being provided.

- eprocedures_03_col:

  Column containing procedure codes with or without procedure names.

- eprocedures_05_col:

  Column containing a count for how many times procedure was attempted.

- eprocedures_06_col:

  Column indicating whether or not procedure was successful.

- confidence_interval:

  Logical. If `TRUE`, the function calculates a confidence interval for
  the proportion estimate.

- method:

  Character. Specifies the method used to calculate confidence
  intervals. Options are `"wilson"` (Wilson score interval) and
  `"clopper-pearson"` (exact binomial interval). Partial matching is
  supported, so `"w"` and `"c"` can be used as shorthand.

- conf.level:

  Numeric. The confidence level for the interval, expressed as a
  proportion (e.g., 0.95 for a 95% confidence interval). Defaults to
  0.95.

- correct:

  Logical. If `TRUE`, applies a continuity correction to the Wilson
  score interval when `method = "wilson"`. Defaults to `TRUE`.

- ...:

  optional additional arguments to pass onto
  [`dplyr::summarize`](https://dplyr.tidyverse.org/reference/summarise.html).

## Value

A data.frame summarizing results for two population groups (Adults and
Peds) with the following columns:

- `pop`: Population type (Adults and Peds).

- `numerator`: Count of incidents meeting the measure.

- `denominator`: Total count of included incidents.

- `prop`: Proportion of incidents meeting the measure.

- `prop_label`: Proportion formatted as a percentage with a specified
  number of decimal places.

- `lower_ci`: Lower bound of the confidence interval for `prop` (if
  `confidence_interval = TRUE`).

- `upper_ci`: Upper bound of the confidence interval for `prop` (if
  `confidence_interval = TRUE`).

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
    evitals_06 = rep(c(90, 100, 102, 103, 104), 2),
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
                           673005), 2),
    eprocedures_05 = rep(1, 10),
    eprocedures_06 = rep(9923003, 10)

  )

# Run the function
# Return 95% confidence intervals using the Wilson method
airway_01(df = NULL,
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
         eprocedures_05_col = eprocedures_05,
         eprocedures_06_col = eprocedures_06,
         earrest_01_col = earrest_01,
         evitals_01_col = evitals_01,
         evitals_06_col = evitals_06,
         evitals_12_col = evitals_12,
         confidence_interval = TRUE
         )
#> 
#> ── Airway-01 ───────────────────────────────────────────────────────────────────
#> 
#> ── Gathering Records for Airway-01 ──
#> 
#> Running `airway_01_population()`  [Working on 1 of 19 tasks] ●●●───────────────…
#> Running `airway_01_population()`  [Working on 2 of 19 tasks] ●●●●──────────────…
#> Running `airway_01_population()`  [Working on 3 of 19 tasks] ●●●●●●────────────…
#> Running `airway_01_population()`  [Working on 4 of 19 tasks] ●●●●●●●───────────…
#> Running `airway_01_population()`  [Working on 5 of 19 tasks] ●●●●●●●●●─────────…
#> Running `airway_01_population()`  [Working on 6 of 19 tasks] ●●●●●●●●●●────────…
#> Running `airway_01_population()`  [Working on 7 of 19 tasks] ●●●●●●●●●●●●──────…
#> Running `airway_01_population()`  [Working on 8 of 19 tasks] ●●●●●●●●●●●●●●────…
#> Running `airway_01_population()`  [Working on 9 of 19 tasks] ●●●●●●●●●●●●●●●───…
#> Running `airway_01_population()`  [Working on 10 of 19 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `airway_01_population()`  [Working on 11 of 19 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `airway_01_population()`  [Working on 12 of 19 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `airway_01_population()`  [Working on 13 of 19 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `airway_01_population()`  [Working on 14 of 19 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `airway_01_population()`  [Working on 15 of 19 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `airway_01_population()`  [Working on 16 of 19 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `airway_01_population()`  [Working on 17 of 19 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `airway_01_population()`  [Working on 18 of 19 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `airway_01_population()`  [Working on 19 of 19 tasks] ●●●●●●●●●●●●●●●●●…
#> 
#> 
#> 
#> ── Calculating Airway-01 ──
#> 
#> 
#> ✔ Function completed in 0.41s.
#> 
#> Warning: In `prop.test()`: Chi-squared approximation may be incorrect for any n < 10.
#> # A tibble: 2 × 8
#>   measure   pop    numerator denominator  prop prop_label lower_ci upper_ci
#>   <chr>     <chr>      <int>       <int> <dbl> <chr>         <dbl>    <dbl>
#> 1 Airway-01 Adults         2           2     1 100%          0.198        1
#> 2 Airway-01 Peds           3           3     1 100%          0.310        1
```
