#' @title Validate Data Structure
#'
#' @description
#' This function checks if an input is of the specified data structure type(s).
#' Depending on the specified type, it will either throw an error, issue a
#' warning, or send a message. It also checks for NULL and NA values based on
#' the specified parameters.
#'
#' @inheritParams validate_numeric
#' @param structure_type A vector of data structure types to check. Possible
#' values are "data.frame", "matrix", "list", "array", "atomic_vector",
#' "tbl_df", "tbl".
#' @param logic The logical operator to use when combining checks. Possible
#' values are "or", and "and".
#' @inheritParams validate_data_pull
#'
#' @return NULL. The function is used for its side effects.
#'
#' @author
#' Nicolas Foss, Ed.D., MS
#'
validate_data_structure <- function(
  input,
  structure_type = c(
    "data.frame",
    "matrix",
    "list",
    "array",
    "atomic_vector",
    "tbl_df",
    "tbl"
  ),
  logic = c("or", "and"),
  type = c("error", "warning", "message"),
  na_ok = TRUE,
  null_ok = TRUE,
  var_name = NULL,
  calls = NULL
) {
  # Validate the type argument
  type <- match.arg(arg = type, choices = c("error", "warning", "message"))

  # Define number of callers to go back
  calls <- ifelse(is.null(calls), 2, calls)

  # Validate the structure_type argument
  structure_type <- match.arg(
    arg = structure_type,
    choices = c(
      "data.frame",
      "matrix",
      "list",
      "array",
      "atomic_vector",
      "tbl_df",
      "tbl"
    ),
    several.ok = TRUE
  )

  # Validate the logic argument
  logic <- match.arg(arg = logic, choices = c("or", "and"))

  # Get the input name, optionally using var_name
  if (is.null(var_name)) {
    input_name <- deparse(substitute(input))
  } else {
    # Validate var_name
    validate_character_factor(input = var_name, type = "error", calls = 1)

    # Initialize input_name using var_name
    input_name <- var_name
  }

  # Check if the input is NULL
  if (is.null(input)) {
    if (!null_ok) {
      validate_error_type(
        input = input_name,
        message = "must not be NULL.",
        type = "error",
        calls = calls
      )
    }
    return(NULL)
  }

  # Check for NA values if na_ok is FALSE
  if (!na_ok && any(is.na(input))) {
    validate_error_type(
      input = input_name,
      message = "must not contain NA values.",
      type = "error",
      calls = calls
    )
  }

  # Perform the checks for each specified data structure type
  checks <- sapply(structure_type, function(type) {
    switch(
      type,
      "data.frame" = is.data.frame(input),
      "tbl" = tibble::is_tibble(input),
      "tbl_df" = tibble::is_tibble(input),
      "matrix" = is.matrix(input),
      "list" = is.list(input),
      "array" = is.array(input),
      "atomic_vector" = is.atomic(input)
    )
  })

  # Combine the results based on the logic argument
  is_valid <- if (logic == "and") {
    sum(checks) == length(checks)
  } else {
    sum(checks) >= 1
  }

  # If the input is not valid, display an error, warning, or message
  if (!is_valid) {
    validate_error_type(
      input = input_name,
      message = glue::glue(
        "must be of class {cli::col_blue(paste0('(', paste(structure_type, collapse = ', '), ')'))}."
      ),
      type = type,
      calls = calls
    )
  }
}
