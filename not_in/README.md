# `%not_in%`

# Description:
The `%not_in%` function is an inverse operator that checks if elements of one vector are not present in another vector. It returns a logical vector indicating the absence of each element.

# Installation
No additional packages are required.

# Usage

```r
result <- x %not_in% y
```

# Arguments

* `x`: A vector whose elements will be checked.
* `y`: A vector against which to check for the presence of elements from x.

# Details

The operator `%not_in%` is a user-friendly alternative to the standard negation of the %in% operator.

# Value
Returns a logical vector indicating `TRUE` for elements of `x` that are not in `y`, and `FALSE` for those that are.

# Example

```r
# Example vectors
x <- c(1, 2, 3, 4)
y <- c(2, 4, 6)

# Using the %not_in% operator
result <- x %not_in% y

# Print result
print(result)  # Output: TRUE FALSE TRUE FALSE
```

# Notes

* This operator is primarily used for improving code readability when performing negation checks.
