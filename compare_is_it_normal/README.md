# `compare_is_it_normal` Function

# Description
The `compare_is_it_normal` function is designed to assess the normality of two numeric variables from the same dataset. It compares these variables using the Shapiro-Wilk test, provides descriptive statistics, and generates visual diagnostics such as Q-Q plots and histograms. This function is a valuable tool for exploratory data analysis when investigating whether two distributions follow a normal distribution.

# Installation
Before using the function, ensure that the required R packages are installed.

```r
install.packages(c("tidyverse", "viridis", "cli"))
```

# Usage

```r
compare_is_it_normal(
  df,
  x1,
  x2,
  data_name = deparse(substitute(df)),
  include_plots = TRUE,
  normalize_x1 = FALSE,
  normalize_x2 = FALSE,
  filter_x1_1 = NULL,
  filter_x1_2 = NULL,
  operator_x1 = NULL,
  filter_x2_1 = NULL,
  filter_x2_2 = NULL,
  operator_x2 = NULL,
  print_out = TRUE,
  plot_theme = theme_cleaner,
  ...
)
```

# Arguments

* `df`: Data frame or tibble containing the variables to compare.
* `x1`: The first numeric variable to assess for normality.
* `x2`: The second numeric variable to assess for normality.
* `data_name`: Optional name of the dataset (default is the df object name).
* `include_plots`: Logical flag to include diagnostic plots (TRUE by default).
* `normalize_x1`: Logical flag to normalize x1 (min-max normalization).
* `normalize_x2`: Logical flag to normalize x2 (min-max normalization).
* `filter_x1_1`: First filtering condition for x1 as a RHS logical argument as a string (e.g. " < 40").
* `filter_x1_2`: Second filtering condition for x1 as a RHS logical argument as a string (e.g. " < 50").
* `operator_x1`: Logical operator for combining filter conditions for x1 as a string (e.g. " & ").
* `filter_x2_1`: First filtering condition for x2 as a RHS logical argument as a string (e.g. " < 40").
* `filter_x2_2`: Second filtering condition for x2 as a RHS logical argument as a string (e.g. " < 50").
* `operator_x2`: Logical operator for combining filter conditions for x2 as a string (e.g. " & ").
* `print_out`: Logical flag to print results to the console.
* `plot_theme`: Custom ggplot2 theme for the diagnostic plots (theme_cleaner by default).
* `...`: Additional arguments passed to internal functions.

# Details
The `compare_is_it_normal` function provides a comprehensive analysis of two numeric variables within the same dataset. It supports the following steps:

* Checks the class of input variables and applies optional filtering.
* Normalizes the variables if required.
* Performs the Shapiro-Wilk test for normality.
* Computes descriptive statistics including mean, standard deviation, and percent missing data.
* Generates diagnostic plots such as Q-Q plots and histograms if `include_plots = TRUE`.

# Value
A tibble with the following columns:

* `variable`: The name of the variable.
* `mean`: The mean of the variable.
* `std_dev`: The standard deviation.
* `minimum`: The minimum value.
* `quantile_25`: The 25th percentile (first quartile).
* `median`: The median value.
* `quantile_75`: The 75th percentile (third quartile).
* `maximum`: The maximum value.
* `non_missing`: The number of non-missing observations.
* `missing`: The number of missing values.
* `percent_missing`: The percentage of missing values.
* `shapiro_test_result`: The p-value of the Shapiro-Wilk test.
* `normality_diagnosis`: A diagnosis of the normality of the distribution based on the Shapiro-Wilk test result.

# Example

```r
# Load libraries
library(tidyverse)
library(viridis)
library(traumaR)

# Sample data
df <- data.frame(
  x1 = rnorm(100),
  x2 = rnorm(100)
)

# Compare the distributions of x1 and x2
result <- compare_is_it_normal(df, x1, x2)

# Print the result
print(result)

# Include diagnostic plots
compare_is_it_normal(df, x1, x2, include_plots = TRUE)
```

# Notes

* When the sample size exceeds 5,000, the Shapiro-Wilk test is conducted on a random sample of 5,000 records to ensure computational efficiency.
* Diagnostic plots (Q-Q plots, histograms) are only generated if include_plots = TRUE.
* Filters can be applied to both variables to limit the range of the data being analyzed, but must be passed as RHS logical arguments as a string (see _Arguments_ above).  If more than one filter is applied, it must be joined by an `operator`, which is a string logical operator such as " & " or " | ".
* This plot uses `theme_cleaner` which is a custom ggplot2 theme created for `traumaR`, and can be accessed via this Github repo.
