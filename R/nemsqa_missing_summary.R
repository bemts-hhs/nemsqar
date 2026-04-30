#' @title Summarize Missingness Across Multiple Data Sources
#'
#' @description
#' Computes missingness statistics for each column in one or more data frames or
#' tibbles. A value is considered missing when it is `NA` or a blank string
#' (`""`). The function returns one row per column per data source.
#'
#' This function is intended for internal data quality workflows within nemsqar.
#'
#' @param ... One or more data frames or tibbles to be evaluated.
#'
#' @return
#' A tibble with one row per column for each input data source. The returned
#' tibble includes:
#' * `data_source`: the name of the object passed to the function.
#' * `column`: the column name.
#' * `n_missing`: the number of missing values (`NA` or `""`).
#' * `n`: the total number of rows in the column.
#' * `prop_missing`: the proportion of missing values.
#' * `prop_missing_label`: the formatted missingness percentage produced by
#'   `nemsqar::pretty_percent()`.
#'
#' @examples
#' df1 <- tibble::tibble(a = c(1, NA), b = c("", "x"))
#' df2 <- tibble::tibble(x = c(NA, NA), y = c("a", ""))
#' nemsqa_missing_summary(df1, df2)
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @keywords internal
#' @noRd
#'
nemsqa_missing_summary <- function(...) {
    # Capture all data sources passed into the function as a list
    inputs <- list(...)

    # Initialize storage for output rows
    out <- list()

    # Capture the full call, including argument names, for labeling data sources
    call <- match.call()

    # Loop over each data source provided
    for (i in seq_along(inputs)) {
        # Extract the ith data.frame or tibble
        data <- inputs[[i]]

        # Derive the user-supplied object name for reporting in `data_source`
        src <- as.character(call[[i + 1]])

        # Obtain the column names of the current data source
        cols <- names(data)

        # Loop over each column and compute missingness metrics
        for (col in cols) {
            # Extract the column vector
            x <- data[[col]]

            # Count values that are NA or blank (""), ignoring NA in comparisons
            if (is.character(x) || is.factor(x)) {
                # Convert factors to character to safely detect blanks
                chr <- as.character(x)
                n_missing <- sum(is.na(chr) | trimws(chr) == "", na.rm = TRUE)
            } else {
                # Numeric, Date, POSIXct, logical, etc.
                n_missing <- sum(is.na(x))
            }
            # Total number of rows in the column
            n_total <- length(x)

            # Append a summary row to the output list
            out[[length(out) + 1]] <- tibble::tibble(
                data_source = src, # name of source object
                column = col, # column name
                n_missing = n_missing, # count of missing or blank
                n = n_total, # total rows
                prop_missing = n_missing / n_total, # proportion missing
                prop_missing_label = pretty_percent(prop_missing, n_decimal = 2)
            )
        }
    }

    # Combine all rows into a single tibble and return
    dplyr::bind_rows(out)
}
