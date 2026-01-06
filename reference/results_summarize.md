# Tabulate Measure Results

Calculates measure numerator, denominator, and proportions for a NEMSQA
measure and each population (total, adult, and pediatric). The function
returns a summarized result table for the selected populations, with
optional confidence intervals for the proportions.

This function is used throughout the package to calculate measure
results for different populations (e.g., total population, adults, and
pediatric groups) based on the given input data. Each of the population
arguments (`total_population`, `adult_population`, `peds_population`)
defaults to `NULL`. If a population argument is `NULL`, it will be
excluded from the results.

## Usage

``` r
results_summarize(
  total_population = NULL,
  adult_population = NULL,
  peds_population = NULL,
  measure_name,
  population_names = c("all", "adults", "peds"),
  numerator_col,
  confidence_interval = FALSE,
  method = c("wilson", "clopper-pearson"),
  conf.level = 0.95,
  correct = TRUE,
  ...
)
```

## Arguments

- total_population:

  A dataframe or tibble containing the filtered and calculated fields
  for the total population. Defaults to `NULL`.

- adult_population:

  A dataframe or tibble containing the filtered and calculated fields
  for the adult population. Defaults to `NULL`.

- peds_population:

  A dataframe or tibble containing the filtered and calculated fields
  for the pediatric population. Defaults to `NULL`.

- measure_name:

  A string containing the description of the measure being calculated.

- population_names:

  A vector of strings specifying which populations (total, adult, peds)
  to include in the result. Default includes all populations.

- numerator_col:

  The tidyselect column containing the numerator data for the measure
  (e.g., the number of cases).

- confidence_interval:

  **\[experimental\]** A logical value indicating whether to include
  confidence intervals in the result. Defaults to FALSE.

- method:

  **\[experimental\]** A string specifying the method to calculate
  confidence intervals. Options are "wilson" or "clopper-pearson".
  Default is "wilson".

- conf.level:

  **\[experimental\]** A numeric value indicating the confidence level
  for the confidence intervals. Default is 0.95 (95% confidence).

- correct:

  **\[experimental\]** A logical value specifying whether to apply
  continuity correction when calculating confidence intervals. Default
  is TRUE.

- ...:

  (optional) Additional arguments passed to the `summarize_measure`
  function used for calculating measure results.

## Value

A tibble containing the summarized measure results for the selected
populations. The output includes:

- `measure`: The measure name.

- `pop`: The population group (e.g., "All", "Adults", "Peds").

- `numerator`: The count of qualifying events.

- `denominator`: The total number of records in the population.

- `prop`: The proportion of qualifying events.

- `prop_label`: A formatted percentage representation of `prop`.

- `lower_ci`, `upper_ci`: The lower and upper confidence interval bounds
  (if `confidence_interval = TRUE`).

If multiple populations are specified, their results are combined into a
single tibble using
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html).

## Author

Samuel Kordik, BBA, BS
