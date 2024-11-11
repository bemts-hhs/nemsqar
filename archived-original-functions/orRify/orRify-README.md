# `orRify`

# Description

The `orRify` function concatenates a vector of strings with the "or" operator (`|`) between each element, producing a single output string. This function is particularly useful for constructing search patterns or queries that require an "or" condition between elements.

# Usage

``` r
orRify(string)
```

# Arguments

-   `string`: A character vector containing the elements to be concatenated with the "or" operator.

# Output

Returns a single concatenated string, with each element separated by a `|` operator.

# Features

-   Processes a character vector to create an "or" delimited string.
-   Simplifies the creation of pattern-matching expressions or query strings.

# Value

A single character string with each element separated by the `|` operator, useful for constructing regex or conditional statements.

# Example

``` r
# Example usage
terms <- c("apple", "banana", "cherry")
orRified_terms <- orRify(terms)
print(orRified_terms)
# Output: "apple|banana|cherry"
```

# Notes

-   The function assumes the input vector has at least one element; if the vector is empty, it will return an empty string.
