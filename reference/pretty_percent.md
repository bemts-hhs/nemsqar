# Format Numeric Variables as Percentages

This function formats numeric variables as percentages with a specified
number of decimal places. It refines the output by removing unnecessary
trailing zeros after the decimal point and ensures the percentage sign
is correctly applied without extraneous characters, resulting in a
polished, human-readable percentage representation.

## Usage

``` r
pretty_percent(variable, n_decimal = 1)
```

## Arguments

- variable:

  A numeric vector representing proportions to format as percentages.
  The values are on a scale from 0 to 1.

- n_decimal:

  A numeric value specifying the number of decimal places. Defaults to
  `1`.

## Value

A character vector containing the formatted percentages.

## Author

Nicolas Foss, Ed.D., MS

## Examples

``` r
# Example usage:
pretty_percent(0.12345)  # Default decimal places
#> [1] "12.3%"
pretty_percent(0.12345, n_decimal = 2)  # Two decimal places
#> [1] "12.35%"
pretty_percent(c(0.1, 0.25, 0.3333), n_decimal = 1)  # Vector input
#> [1] "10%"   "25%"   "33.3%"
```
