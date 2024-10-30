# `pretty_percent`

# Description
The `pretty_percent` function formats numeric values as percentages, removing unnecessary trailing zeros and ensuring a clean output. This enhances the readability of percentage values in reports or visualizations.

# Installation
Ensure that the scales package is installed, as it is required for this function to operate.

```r
library(scales)
```

# Usage

```r
pretty_percent(variable, n_decimal = 0.1)
```

# Arguments

* `variable`: A numeric vector representing the values to be formatted as percentages.
* `n_decimal`: The accuracy (number of decimal places, scale from 0 to 1) to which the percentage is formatted (default is 0.1).

# Details

* The function utilizes the percent function from the scales package to format the input as a percentage.
* It removes trailing zeros after the decimal point and simplifies formatting by eliminating unnecessary characters.

# Value

Returns a character vector of formatted percentage strings.

# Example

```r
# Example usage with more complex numbers
# Dividing values to create percentages
numerator <- c(7.5, 20.0, 0.3333, 100.0, 15.75, 0.005, 150.0)
denominator <- c(100.0, 100.0, 1.0, 100.0, 100.0, 0.1, 300.0)

percent_values <- numerator / denominator  # This will create values like 0.075, 0.20, etc.
formatted_percent <- pretty_percent(percent_values, n_decimal = 0.01)
print(formatted_percent)  
# Output: "7.5%", "20%", "33.33%", "100%", "15.75%", "5%", "50%"
```

# Notes

* This function is particularly useful for presenting percentage data in a more concise and visually appealing manner.
