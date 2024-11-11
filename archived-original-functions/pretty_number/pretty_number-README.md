# `pretty_number`

# Description
The `pretty_number` function formats large numeric values into a more readable string representation with optional decimal rounding and prefix support. It converts numbers into a compact format using standard suffixes (e.g., "k" for thousand, "m" for million).

# Installation

Ensure the `tidyverse` package is installed, as it is required for this function to operate.

```r
install.packages("tidyverse")
```

# Usage

```r
pretty_number(x, n_decimal = 2, prefix = NULL, round = TRUE)
```

# Arguments

* `x`: A numeric vector to be formatted.
* `n_decimal`: The number of decimal places to round to (default is 2).
* `prefix`: An optional character string to prepend to the formatted number.
* `round`: A logical value indicating whether to round the numbers (default is `TRUE`).

# Details

The function handles large numbers, converting them into more manageable formats based on their magnitude.
It uses suffixes to indicate the scale of the number.

# Value
Returns a character vector of formatted numbers.

# Example

```r
# Example usage
formatted_number <- pretty_number(1500)
print(formatted_number)  # Output: "1.5k"

formatted_with_prefix <- pretty_number(2500000, prefix = "$")
print(formatted_with_prefix)  # Output: "$2.5m"
```

# Notes

* The function temporarily adjusts the scipen option to allow for the formatting of very long numbers without scientific notation. This setting is restored after the function completes.
