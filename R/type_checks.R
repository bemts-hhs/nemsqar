#' Checks if object is data frame or tibble
#'
#' Ensures the passed object or objects are dataframes or tibbles and aborts with
#' an error if one of them is not.
#'
#' @param ... one or more objects to test
#' @param .abort_error Aborts with CLI message if any of the supplied objects are not dataframes or tibbles
#'
#' @return boolean values for each item(s) in .data

check_is_tbl_df <- function(...,
                            .abort_error = TRUE) {

  check_list <- list(...)

  results <- sapply(check_list, is.data.frame) | sapply(check_list, tibble::is_tibble)

  # Get the call of the parent function
  parent_call <- sys.call(-1)

  # Extract the name of the function from the call
  if (!is.null(parent_call)) {
    function_name <- as.character(parent_call[[1]])
  } else {
    function_name <- "this function"
  }

  if(!all(results) && .abort_error) {
    cli::cli_abort(paste0("One or more of the tables passed to {.fn ",function_name,"} were not of class {.cls data.frame} nor {.cls tibble}. When passing multiple tables to {.fn ",function_name,"}, all tables must be of class {.cls data.frame} or {.cls tibble}."))
  }

  return(results)

}

#' Checks if object is Date or POSIXct
#'
#' Ensures the passed vector or vectors are dates.
#' an error if one of them is not.
#'
#' @param ... one or more objects to test
#' @param .abort_error Aborts with CLI message if any of the supplied objects are not dataframes or tibbles
#'
#' @return boolean values for each item(s) in .data
check_is_date <- function(...,
                          .abort_error = TRUE) {

  check_list <- list(...)

  results <- sapply(check_list, lubridate::is.Date) | sapply(check_list, lubridate::is.POSIXct)

  # Get the call of the parent function
  parent_call <- sys.call(-1)

  # Extract the name of the function from the call
  if (!is.null(parent_call)) {
    function_name <- as.character(parent_call[[1]])
  } else {
    function_name <- "this function"
  }

  if(!all(results) && .abort_error) {
    cli::cli_abort(paste0("One or more of the date variables passed to {.fn ",function_name,"} were not of class {.cls Date} nor {.cls POSIXct}. When passing multiple tables to {.fn ",function_name,"}, all tables must be of class {.cls data.frame} or {.cls tibble}."))
  }

  return(results)

}

