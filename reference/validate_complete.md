# Validate Complete Input

This function checks if the input contains any missing values (NA).
Depending on the specified type, it will either throw an error, issue a
warning, or send a message. It also checks for NULL values based on the
specified parameters.

## Usage

``` r
validate_complete(
  input,
  type = c("error", "warning", "message"),
  null_ok = TRUE,
  var_name = NULL,
  calls = NULL
)
```

## Arguments

- input:

  The data to be validated.

- type:

  A character string specifying the type of message to be displayed if
  the input is not numeric or if the values are out of range. Must be
  one of "error", "warning", or "message".

- null_ok:

  Logical. If TRUE, NULL values are allowed. Default is TRUE.

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
