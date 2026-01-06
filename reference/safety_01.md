# Safety-01 Calculation

The `safety_01` function calculates the proportion of 911 responses
where "lights and sirens" were not used in an EMS dataset. It generates
age-based population summaries, calculating the count and proportion of
"lights and sirens" responses among all incidents, and within adult and
pediatric groups.

## Usage

``` r
safety_01(
  df = NULL,
  patient_scene_table = NULL,
  response_table = NULL,
  erecord_01_col,
  incident_date_col = NULL,
  patient_DOB_col = NULL,
  epatient_15_col,
  epatient_16_col,
  eresponse_05_col,
  eresponse_24_col,
  confidence_interval = FALSE,
  method = c("wilson", "clopper-pearson"),
  conf.level = 0.95,
  correct = TRUE,
  ...
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

  Column that contains the incident date. This defaults to `NULL` as it
  is optional in case not available due to PII restrictions.

- patient_DOB_col:

  Column that contains the patient's date of birth. This defaults to
  `NULL` as it is optional in case not available due to PII
  restrictions.

- epatient_15_col:

  Column containing age.

- epatient_16_col:

  Column for age units.

- eresponse_05_col:

  Column containing response mode codes (e.g., 911 response codes).

- eresponse_24_col:

  Column detailing additional response descriptors as text.

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
    eresponse_24 = rep("No Lights or Sirens", 5)
  )

# Run the function
# Return 95% confidence intervals using the Wilson method
  safety_01(
    df = test_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    eresponse_24_col = eresponse_24,
    confidence_interval = TRUE
  )
#> 
#> ── Safety-01 ───────────────────────────────────────────────────────────────────
#> 
#> ── Gathering Records for Safety-01 ──
#> 
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
#> 
#> 
#> ── Calculating Safety-01 ──
#> 
#> 
#> ✔ Function completed in 0.18s.
#> 
#> Warning: In `prop.test()`: Chi-squared approximation may be incorrect for any n < 10.
#> # A tibble: 3 × 8
#>   measure   pop    numerator denominator  prop prop_label lower_ci upper_ci
#>   <chr>     <chr>      <int>       <int> <dbl> <chr>         <dbl>    <dbl>
#> 1 Safety-01 Adults         3           3     1 100%         0.310         1
#> 2 Safety-01 Peds           1           1     1 100%         0.0546        1
#> 3 Safety-01 All            5           5     1 100%         0.463         1
```
