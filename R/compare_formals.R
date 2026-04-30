#' @title
#' Compare formals across functions
#'
#' @description
#' Identify which arguments of a target function are unique and which are shared
#' with a set of comparison functions. Useful for determining whether roxygen2's
#' `@inheritParams` is appropriate.
#'
#' @param fun A function object to analyze. Do not quote the function name.
#' @param others A list of function objects to compare against.
#'
#' @return A list with two elements:
#' * `shared`: Character vector of argument names that appear in both the target
#'   function and at least one comparison function.
#'  * unique: Character vector of argument names that appear only in the target
#'    function.
#'
#' @keywords internal
#'
#' @noRd
#'
#' @author
#' Nicolas Foss, Ed.D., MS
#'
compare_formals <- function(fun, others) {
  target <- names(formals(fun))
  other_args <- unique(unlist(lapply(others, \(x) names(formals(x)))))

  list(
    shared = intersect(target, other_args),
    unique = setdiff(target, other_args)
  )
}
