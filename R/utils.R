#' Not In operator
#'
#' The `%not_in%` function is an inverse operator that checks if elements of one
#' vector are not present in another vector. It returns a logical vector
#' indicating the absence of each element.
#'
#' The operator `%not_in%` is a user-friendly alternative to the standard negation
#' of the %in% operator.
#'
#' @param x A vector whose elements will be checked.
#' @param y A vector against which to check for the presence of elements from x.
#'
#' @return Returns a logical vector indicating `TRUE` for elements of `x` that
#'         are not in `y`, and `FALSE` for those that are.
#' @export
#'
#' @examples
#' x <- c(1, 2, 3, 4)
#' y <- c(2, 4, 6)
#'
#' x %not_in% y
#' # TRUE FALSE TRUE FALSE
#'
`%not_in%` <- function(x, y) {
  !(x %in% y)

}

#' orRify
#'
#' The `orRify` function concatenates a vector of strings with the "or" operator
#' (`|`) between each element, producing a single output string. This function
#' is particularly useful for constructing search patterns or queries that r
#' equire an "or" condition between elements.
#'
#' The function assumes the input vector has at least one element; if the vector
#' is empty, it will return an empty string.
#'
#' @param string A character vector containing the elements to be concatenated with the "or" operator.
#'
#' @return Returns a single concatenated string, with each element separated by a `|` operator.
#' @export
#'
#' @examples
#' terms <- c("apple", "banana", "cherry")
#' orRified_terms <- orRify(terms)
#' print(orRified_terms)
#' # "apple|banana|cherry"
#'
orRify <- function(string){

  out <- character()

  for (i in 1:(length(string) - 1)) {

    a <- string[i]

    out <- paste(out, a, "|", sep = "")

  }

  out <- paste(out, string[length(string)], sep = "")

  out

}

#' Generate Random ID
#'
#' Generate a random set of letters and numbers to create a random ID.
#'
#' The `generate_random_ID` function generates a specified number of random IDs.
#' Each ID consists of 10 randomly chosen upper/lower case letters followed by a
#'  10-digit random number. Optionally, a seed can be set for reproducibility.
#'
#' @param n Integer. The number of random IDs to generate.
#' @param set_seed Numeric or `NULL`. The seed value for random number
#'                 generation. If a numeric value is provided, the seed is set
#'                 to ensure reproducibility. If `NULL`, no seed is set, and the
#'                 results will vary across executions.
#'
#' @return Returns a character vector containing `n` randomly generated IDs.
#'        Each ID is formatted as a string of 10 letters followed by a hyphen
#'        and a 10-digit number.

#' @export
#'
#' @examples
#' # Generate 5 random IDs with a seed for reproducibility
#' random_ids <- generate_random_ID(5, set_seed = 42)
#' print(random_ids)
#'
#' # Generate 3 random IDs without setting a seed
#' random_ids_no_seed <- generate_random_ID(3, set_seed = NULL)
generate_random_ID <- function(n, set_seed = 12345) {
  # choose whether or not to set the seed

  if (is.numeric(set_seed)) {
    set.seed(set_seed)

  } else if (is.null(set_seed)) {
    message(
      "Set.seed was not called internally, and so your results will not be consistent across applications.  Make [set_seed] any number to set the seed and make reproducible results!"
    )

  }

  random_strings <- vector(mode = "character", length = n)
  for (i in 1:n) {
    random_strings[i] <- paste0(paste0(sample(c(
      LETTERS, letters, LETTERS
    ), size = 10), collapse = ""),
    "-",
    sample(1000000000:9999999999, size = 1))
  }
  return(random_strings)
}
