# Validate Class

This function checks if the input is of the specified class type(s).
Depending on the specified type, it will either throw an error, issue a
warning, or send a message. It also checks for NULL and NA values based
on the specified parameters.

## Usage

``` r
validate_class(
  input,
  class_type = c("numeric", "integer", "logical", "character", "factor", "complex",
    "raw", "date", "date-time", "hms"),
  logic = c("or", "and"),
  type = c("error", "warning", "message"),
  na_ok = TRUE,
  null_ok = TRUE,
  finite = FALSE,
  var_name = NULL,
  calls = NULL
)
```

## Arguments

- input:

  The data to be validated.

- class_type:

  A vector of class types to check. Possible values are "numeric",
  "integer", "logical", "character", "factor", "complex", "raw", "date",
  "date-time", "hms".

- logic:

  The logical operator to use when combining checks. Possible values are
  "or", and "and".

- type:

  A character string specifying the type of message to be displayed if
  the input is not numeric or if the values are out of range. Must be
  one of "error", "warning", or "message".

- na_ok:

  Logical. If TRUE, NA values are allowed. Default is TRUE.

- null_ok:

  Logical. If TRUE, NULL values are allowed. Default is TRUE.

- finite:

  Logical. If TRUE, only finite values are allowed. Default is FALSE.

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
