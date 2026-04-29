# Validate Set Equality

This function checks if all elements of an input are within a specified
set of valid values. Depending on the specified type, it will either
throw an error, issue a warning, or send a message.

## Usage

``` r
validate_set(
  input,
  valid_set,
  type = c("error", "warning", "message"),
  na_ok = TRUE,
  null_ok = TRUE,
  var_name = NULL,
  calls = NULL
)
```

## Arguments

- input:

  The data to be validated.

- valid_set:

  A vector of valid values.

- type:

  A character string specifying the type of message to be displayed if
  the input is not numeric or if the values are out of range. Must be
  one of "error", "warning", or "message".

- na_ok:

  Logical. If TRUE, NA values are allowed. Default is TRUE.

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
