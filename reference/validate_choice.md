# Validate Choice

This function checks if an input is within a specified set of valid
choices. Depending on the specified type, it will either throw an error,
issue a warning, or send a message.

## Usage

``` r
validate_choice(
  input,
  choices,
  several.ok = FALSE,
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

- choices:

  a character vector of candidate values, often missing, see
  documentation for
  [`base::match.arg()`](https://rdrr.io/r/base/match.arg.html) for more
  information.

- several.ok:

  logical specifying if `arg` should be allowed to have more than one
  element.

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

The validated input if it is valid.

## Details

This function uses [`match.arg`](https://rdrr.io/r/base/match.arg.html)
to validate the input against the allowed choices. Please see the
documentation for [`match.arg`](https://rdrr.io/r/base/match.arg.html)
for more details about how matching is performed.

## Author

Nicolas Foss, Ed.D., MS
