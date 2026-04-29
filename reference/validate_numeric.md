# Validate Numeric Input

This function checks if an input is numeric and optionally checks if the
values are within a specified range. Depending on the specified type, it
will either throw an error, issue a warning, or send a message.
Additional arguments allow for checking NA values, NULL values, and
finite values.

## Usage

``` r
validate_numeric(
  input,
  min = NULL,
  max = NULL,
  na_ok = TRUE,
  null_ok = TRUE,
  finite = FALSE,
  type = c("error", "warning", "message"),
  var_name = NULL,
  calls = NULL
)
```

## Arguments

- input:

  The data to be validated.

- min:

  Optional. The minimum value for the range check.

- max:

  Optional. The maximum value for the range check.

- na_ok:

  Logical. If TRUE, NA values are allowed. Default is TRUE.

- null_ok:

  Logical. If TRUE, NULL values are allowed. Default is TRUE.

- finite:

  Logical. If TRUE, only finite values are allowed. Default is FALSE.

- type:

  A character string specifying the type of message to be displayed if
  the input is not numeric or if the values are out of range. Must be
  one of "error", "warning", or "message".

- var_name:

  Optional. A character string giving the desired variable (or object)
  name that will appear in console output in place of the how the object
  will typically be named in messages via deparse(substitute(input)).

- calls:

  Optional. The number of callers to go back in the call stack for error
  messaging. If NULL, will default to 2.

## Value

NULL. The function is used for its side effects.

## Author

Nicolas Foss, Ed.D., MS
