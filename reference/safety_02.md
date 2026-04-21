# Safety-02 Calculation

The `safety_02` function calculates the Safety-02 metric, evaluating the
proportion of emergency medical calls involving transport where no
lights and sirens were used. This function categorizes the population
into adult and pediatric groups based on their age, and summarizes
results with a total population count as well.

## Usage

``` r
safety_02(
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
  transport_disposition_cols,
  confidence_interval = FALSE,
  method = c("wilson", "clopper-pearson"),
  conf.level = 0.95,
  correct = TRUE,
  ...
)
```

## Arguments

- df:

  A data frame where each row is an observation, and each column
  represents a feature.

- patient_scene_table:

  A data.frame or tibble containing only epatient and escene fields as a
  fact table.

- response_table:

  A data.frame or tibble containing only the eresponse fields needed for
  this measure's calculations.

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

  Column giving the calculated age value.

- epatient_16_col:

  Column giving the provided age unit value.

- eresponse_05_col:

  Column giving response codes, identifying 911 responses.

- edisposition_18_col:

  Column giving transport mode descriptors, including possible
  lights-and-sirens indicators.

- edisposition_28_col:

  Column giving patient evaluation and care categories for the EMS
  response.

- transport_disposition_cols:

  One or more unquoted column names (such as edisposition.12,
  edisposition.30) containing transport disposition details.

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
    edisposition_18 = rep(4218015, 5),
    edisposition_28 = rep(4228001, 5),
    edisposition_30 = rep(4230001, 5)
  )

# Run the function
# Return 95% confidence intervals using the Wilson method
  safety_02(
    df = test_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    edisposition_18_col = edisposition_18,
    edisposition_28_col = edisposition_28,
    transport_disposition_cols = edisposition_30,
    confidence_interval = TRUE
  )
#> 
#> ── Safety-02 ───────────────────────────────────────────────────────────────────
#> 
#> ── Gathering Records for Safety-02 ──
#> 
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
#> 
#> 
#> ── Calculating Safety-02 ──
#> 
#> 
#> ✔ Function completed in 0.19s.
#> 
#> Warning: In `prop.test()`: Chi-squared approximation may be incorrect for any n < 10.
#> # A tibble: 3 × 8
#>   measure   pop    numerator denominator  prop prop_label lower_ci upper_ci
#>   <chr>     <chr>      <int>       <int> <dbl> <chr>         <dbl>    <dbl>
#> 1 Safety-02 Adults         3           3     1 100%         0.310         1
#> 2 Safety-02 Peds           1           1     1 100%         0.0546        1
#> 3 Safety-02 All            5           5     1 100%         0.463         1
```
