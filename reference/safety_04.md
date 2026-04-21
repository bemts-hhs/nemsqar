# Safety-04 Calculation

The `safety_04` function processes EMS incident data for specific safety
and transport criteria, filtering by patient age and incident type to
identify cases that meet specified exclusion or inclusion criteria. This
function accommodates data with various EMS-specific codes, age
descriptors, and procedure identifiers.

## Usage

``` r
safety_04(
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
  transport_disposition_col,
  confidence_interval = FALSE,
  method = c("wilson", "clopper-pearson"),
  conf.level = 0.95,
  correct = TRUE,
  ...
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

- confidence_interval:

  **\[experimental\]** Logical. If `TRUE`, the function calculates a
  confidence interval for the proportion estimate.

- method:

  **\[experimental\]**Character. Specifies the method used to calculate
  confidence intervals. Options are `"wilson"` (Wilson score interval)
  and `"clopper-pearson"` (exact binomial interval). Partial matching is
  supported, so `"w"` and `"c"` can be used as shorthand.

- conf.level:

  **\[experimental\]**Numeric. The confidence level for the interval,
  expressed as a proportion (e.g., 0.95 for a 95% confidence interval).
  Defaults to 0.95.

- correct:

  **\[experimental\]**Logical. If `TRUE`, applies a continuity
  correction to the Wilson score interval when `method = "wilson"`.
  Defaults to `TRUE`.

- ...:

  optional additional arguments to pass onto
  [`dplyr::summarize`](https://dplyr.tidyverse.org/reference/summarise.html).

## Value

A data.frame summarizing results for two population groups (All, Adults
and Peds) with the following columns:

- `pop`: Population type (All, Adults, and Peds).

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

Nicolas Foss, Ed.D., MS

## Examples

``` r
# Synthetic test data
  test_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    earrest_01 = rep("No", 5),
    einjury_03 = rep("non-injury", 5),
    edisposition_14 = rep(4214001, 5),
    edisposition_30 = rep(4230001, 5),
    eprocedures_03 = rep("other response", 5)
  )

# Run the function
# Return 95% confidence intervals using the Wilson method
  safety_04(
    df = test_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    earrest_01_col = earrest_01,
    einjury_03_col = einjury_03,
    edisposition_14_col = edisposition_14,
    transport_disposition_col = edisposition_30,
    eprocedures_03_col = eprocedures_03,
    confidence_interval = TRUE
  )
#> 
#> ── Safety-04 ───────────────────────────────────────────────────────────────────
#> 
#> ── Gathering Records for Safety-04 ──
#> 
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
#> 
#> 
#> ── Calculating Safety-04 ──
#> 
#> 
#> ✔ Function completed in 0.22s.
#> 
#> Warning: In `prop.test()`: Chi-squared approximation may be incorrect for any n < 10.
#> # A tibble: 1 × 8
#>   measure   pop   numerator denominator  prop prop_label lower_ci upper_ci
#>   <chr>     <chr>     <int>       <int> <dbl> <chr>         <dbl>    <dbl>
#> 1 Safety-04 Peds          2           2     1 100%          0.198        1
```
