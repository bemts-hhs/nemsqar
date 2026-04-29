# Validate Data Extraction

This function extracts a column from a data frame or tibble and returns
it as a vector. If the column does not exist or an error occurs, it
returns a clean error message using the cli package.

## Usage

``` r
validate_data_pull(
  input,
  type = c("error", "warning", "message"),
  col,
  var_name = NULL,
  calls = NULL
)
```

## Arguments

- input:

  A data frame or tibble.

- type:

  A character string specifying the type of message to be displayed if
  the input is not numeric or if the values are out of range. Must be
  one of "error", "warning", or "message".

- col:

  The column to be extracted.

- var_name:

  Optional. The name of the variable for error messaging.

- calls:

  Optional. The number of callers to go back in the call stack for error
  messaging. If NULL, will default to 2.

## Value

The extracted column as a vector.

## Details

This function is designed to validate and extract a specified column
from a data frame or tibble. When using `validate_data_pull()` within
custom functions, it is necessary to call a given bare column name using
tidy evaluation (e.g., `{{ col }}`). This allows the function to
correctly capture and evaluate the column name within the custom
function. However, when calling this function directly on a data frame,
tidy evaluation is not required.

## Author

Nicolas Foss, Ed.D., MS
