# `is_it_normal`

# Description

The `is_it_normal` function is designed to assess whether a continuous numeric variable in a dataset follows a normal distribution. This function provides key descriptive statistics, performs a Shapiro-Wilk test for normality, and offers a range of visualizations such as Q-Q plots, histograms, kernel density plots, and boxplots to help you evaluate the distribution of the variable.

# Installation

You can install the required package for this function by installing the `tidyverse` package:

```r
install.packages("tidyverse")
```

# Usage

```r
is_it_normal(df, 
             x, 
             include_plots = TRUE, 
             filter1 = NULL, 
             filter2 = NULL, 
             operator = NULL, 
             scale = FALSE, 
             print_out = TRUE, 
             plot_theme = theme_cleaner, 
             ...)
```

# Arguments

* `df`: A dataframe or tibble containing the variable to be analyzed.
* `x`: (Required) The unquoted name of the numeric variable to test for normality.
* `include_plots`: (Logical) Whether to include visualizations such as Q-Q plot, histogram, density plot, and boxplot. Default is TRUE.
* `filter1`: (Optional) A condition to filter the dataframe based on the variable. Used when you want to filter data before the analysis.  Must be a RHS logical argument as a string (e.g. " < 40").
* `filter2`: (Optional) A second condition for filtering.  Must be a RHS logical argument as a string (e.g. " > 50").
* `operator`: (Optional) A logical operator (e.g., &, |) for more complex filtering conditions as a string to join `filter1` and `filter2`.
* `scale`: (Logical) Whether to apply Min-Max normalization to the variable before the analysis. Default is FALSE.
* `print_out`: (Logical) Whether to print the output tibble or return it invisibly. Default is TRUE.
* `plot_theme`: (Optional) A ggplot2 theme function name to apply custom styling to the plots. Default is theme_cleaner.
* `...`: Additional arguments passed to ggplot2 themes or other customization.

# Details

The is_it_normal function provides a comprehensive way to check whether a continuous variable in a dataset is normally distributed. It performs several key tasks:

* Filters the dataframe based on provided conditions.
* Computes descriptive statistics such as mean, standard deviation, quartiles, and missing data information.
* Conducts a Shapiro-Wilk test of normality.
* Optionally applies Min-Max scaling to the data.
* Generates several useful visualizations including:
* Q-Q Plot: to visualize the quantiles against a theoretical normal distribution.
* Histogram: to observe the shape of the distribution.
* Kernel Density Plot: to estimate the probability density function.
* Boxplot: to summarize the distribution through median and quartiles.

# Value
* Output: A `tibble` containing:

* Mean
* Standard Deviation
* Minimum, Maximum, and Quartiles
* Number of missing values
* Shapiro-Wilk p-value
* Normality diagnosis ("Data are normally distributed" or "Data are not normally distributed").
* Plots: If `include_plots = TRUE`, the function will print several `ggplot2` visualizations to assess normality.

# Example

```r
# Example usage
library(ggplot2)
library(viridis)

# Sample data
data(mtcars)

# Checking if the 'mpg' variable is normally distributed
is_it_normal(mtcars, mpg)

# Using a filter and custom theme
is_it_normal(mtcars, mpg, filter1 = " > 15", filter2 = " < 100", operator = " & ", plot_theme = theme_minimal)
```

# Notes

* The function requires the tidyverse package for data manipulation and visualization.
* For very large datasets (n > 5000), the function samples 5000 data points to run the Shapiro-Wilk test, as it is designed for smaller sample sizes.
* Ensure the x variable is continuous (numeric or integer) to avoid errors.
* Filters are passed as RHS logical arguments which are strings like " < 50" and if more than one filter is applied, they must be joined by an `operator` which is a logical operator like " & " or " | ".
* `theme_cleaner` can be accessed via this Github repo.
