# Validate Error Type

This function displays an error, warning, or message based on the
specified type.

## Usage

``` r
validate_error_type(
  input,
  message,
  type = c("error", "warning", "message"),
  calls = NULL
)
```

## Arguments

- input:

  The data to be validated.

- message:

  The message to be displayed.

- type:

  A character string specifying the type of message to be displayed if
  the input is not numeric or if the values are out of range. Must be
  one of "error", "warning", or "message".

- calls:

  Optional. The number of callers to go back in the call stack for error
  messaging. If NULL, will default to 2.

## Value

NULL. The function is used for its side effects.
