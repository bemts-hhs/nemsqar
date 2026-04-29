# Compare formals across functions

Identify which arguments of a target function are unique and which are
shared with a set of comparison functions. Useful for determining
whether roxygen2's `@inheritParams` is appropriate.

## Usage

``` r
compare_formals(fun, others)
```

## Arguments

- fun:

  A function object to analyze. Do not quote the function name.

- others:

  A list of function objects to compare against.

## Value

A list with two elements:

- `shared`: Character vector of argument names that appear in both the
  target function and at least one comparison function.

- unique: Character vector of argument names that appear only in the
  target function.

## Author

Nicolas Foss, Ed.D., MS
