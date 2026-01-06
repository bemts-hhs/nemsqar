# Summarize Measure

Calculates measure numerator, denominator, proportions, and optional
confidence intervals for a NEMSQA measure. This function summarizes the
information for a specified population and measure, returning a tibble
with the calculated values. If requested, the function can also
calculate confidence intervals for the proportions using either the
Wilson score interval or the Clopper-Pearson exact binomial interval.

## Usage

``` r
summarize_measure(
  data,
  measure_name,
  population_name,
  numerator_col,
  confidence_interval = FALSE,
  method = c("wilson", "clopper-pearson"),
  conf.level = 0.95,
  correct = TRUE,
  ...
)
```

## Arguments

- data:

  A dataframe or tibble containing the filtered and calculated fields
  for the population of interest.

- measure_name:

  A string containing the description of the measure being calculated
  (e.g., "Airway-01").

- population_name:

  A string containing the description of the population for which the
  measure is being calculated (e.g., "Adults", "Peds", or "All").

- numerator_col:

  The tidyselect column containing the numerator data for the measure
  (e.g., the number of cases).

- confidence_interval:

  **\[experimental\]** A logical value indicating whether to calculate a
  confidence interval for the proportion estimate. Defaults to `FALSE`.

- method:

  **\[experimental\]** A string specifying the method to calculate the
  confidence intervals. Options are `"wilson"` (Wilson score interval)
  or `"clopper-pearson"` (exact binomial interval). Partial matching is
  allowed (e.g., `"w"` or `"c"`). Default is `"wilson"`.

- conf.level:

  **\[experimental\]** A numeric value indicating the confidence level
  for the interval, expressed as a proportion (e.g., 0.95 for a 95%
  confidence interval). Defaults to 0.95.

- correct:

  **\[experimental\]** A logical value specifying whether to apply
  continuity correction to the Wilson score interval when
  `method = "wilson"`. Default is `TRUE`.

- ...:

  (optional) Additional arguments passed to
  [`dplyr::summarize()`](https://dplyr.tidyverse.org/reference/summarise.html)
  when calculating estimates for groups via `.by`.

## Value

A summarized data frame containing:

- `measure`: The measure name.

- `pop`: The population group.

- `numerator`: The count of qualifying events.

- `denominator`: The total count of records.

- `prop`: The proportion of qualifying events.

- `prop_label`: A formatted percentage representation of `prop` (when
  `confidence_interval = FALSE`).

- `lower_ci`, `upper_ci`: The lower and upper confidence interval bounds
  (when `confidence_interval = TRUE`).

## Author

Samuel Kordik, BBA, BS
