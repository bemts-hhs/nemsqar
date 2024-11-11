# `compare_grouped_is_it_normal`

# Description

The `compare_grouped_is_it_normal` function allows users to analyze the normality of a numeric variable across groups defined by a factor variable. It performs Shapiro-Wilk normality tests and provides descriptive statistics and visualizations (including Q-Q plots, histograms, kernel density plots, and boxplots). Users can apply filters, scale the data using min-max normalization, and customize the output with plot themes.

## Usage

```r
compare_grouped_is_it_normal(
  df,
  data_name = deparse(substitute(df)),
  x,
  group_var,
  include_plots = TRUE,
  filter1 = NULL,
  filter2 = NULL,
  operator = NULL,
  scale = FALSE,
  print_out = TRUE,
  plot_theme = theme_cleaner,
  ...
)
```

# Arguments

* `df`: The input dataframe containing the data to be analyzed either piped in or passed as an unquoted object name.
* `data_name`: Optional. The name of the dataframe. Defaults to the expression used for the df argument.  If user-supplied, must be a string vector of length 1.
* `x`: The numeric variable to analyze for normality as an unquoted column name.
* `group_var`: The factor variable used for grouping the data as an unquoted column name.
* `include_plots`: Logical. Whether to include plots in the output. Defaults to TRUE.
* `filter1`: Optional. The first filter condition for the numeric variable, must be a RHS logical argument as a string (e.g. " < 50").
* `filter2`: Optional. The second filter condition for the numeric variable, must be a RHS logical argument as a string (e.g. " < 100").
* `operator`: Optional. The operator to combine filter1 and filter2.
* `scale`: Logical. Whether to apply min-max normalization to the numeric variable. Defaults to FALSE.
* `print_out`: Logical. Whether to print the results to the console. Defaults to TRUE.
* `plot_theme`: A function that defines the theme for the plots. Defaults to theme_cleaner.
* `...`: Additional arguments passed to the plot_theme.

# Output

**Descriptive Statistics**: Summary statistics including mean, standard deviation, minimum, quartiles, median, maximum, and missing data information for each group.
**Shapiro-Wilk Test Results**: P-values for the normality tests, along with a diagnosis of normal or non-normal distribution.
**Visualizations**: Optional plots such as Q-Q plots, histograms, kernel density plots, and boxplots for each group.

```r
# Example dataframe
df <- data.frame(
  score = rnorm(100, mean = 50, sd = 10),
  group = factor(sample(c("A", "B", "C"), 100, replace = TRUE))
)

# Running the function
compare_grouped_is_it_normal(df, x = score, group_var = group)
```

# Features
* **Normality Testing**: Uses the Shapiro-Wilk test for normality.
* **Filtering**: Users can filter the data based on conditions using the filter1, filter2, and operator arguments.
* **Scaling**: Optional min-max normalization for the numeric variable.
* **Customizable Plot Themes**: Allows users to apply custom ggplot2 themes to the output plots.
* **Comprehensive Descriptive Statistics**: Provides summary statistics across all groups.

# Value

Value
The function returns the following components:

* `descriptive_stats`: A dataframe containing the descriptive statistics (mean, median, standard deviation, etc.) for the numeric variable within each group.
* `shapiro_test_results`: A dataframe with the results of the Shapiro-Wilk test for normality, including p-values and a normality diagnosis (Normal/Non-normal).
* `plots`: A list of ggplot2 objects representing the Q-Q plots, histograms, kernel density plots, and boxplots for each group (if include_plots = TRUE).
* `print_output`: Printed results, including the descriptive statistics and normality tests, if print_out = TRUE.

# Example

```r
# Load necessary libraries
library(tidyverse)

# Create example dataframe
df <- data.frame(
  score = rnorm(100, mean = 50, sd = 10),
  group = factor(sample(c("A", "B", "C"), 100, replace = TRUE))
)

# Running the function without scaling
result <- compare_grouped_is_it_normal(df, x = score, group_var = group)

# Access the descriptive statistics
result

# Display one of the plots (if generated)
result$plots[[1]]

# Running the function with scaling and filtering
result_scaled_filtered <- compare_grouped_is_it_normal(
  df,
  x = score,
  group_var = group,
  filter1 = " > 40", # filters must be a quoted logical argument
  scale = TRUE
)

# Access filtered and scaled descriptive statistics
result_scaled_filtered
```

# Notes
* Requires the `tidyverse` and `viridis` packages for data manipulation and visualizations.
* Designed to handle large datasets by limiting the Shapiro-Wilk test to a sample of 5000 records when necessary.
