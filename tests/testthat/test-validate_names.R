testthat::test_that("validate_names() passes silently when all names are valid", {
  test_fun <- function(df, groups) {
    validate_names(input = df, check_names = groups)
    df |> dplyr::select(dplyr::all_of(groups))
  }

  df <- data.frame(a = 1, b = 2)

  testthat::expect_silent(
    test_fun(df, groups = c("a", "b"))
  )
})

testthat::test_that("validate_names() errors when invalid names (small set)", {
  test_fun <- function(df, groups) {
    validate_names(input = df, check_names = groups, type = "error")
    df
  }

  df <- data.frame(a = 1, b = 2)

  testthat::expect_error(
    test_fun(df, groups = c("a", "c")),
    regexp = "invalid column names"
  )
})

testthat::test_that("validate_names() issues warning when type='warning'", {
  test_fun <- function(df, groups) {
    validate_names(input = df, check_names = groups, type = "warning")
    df
  }

  df <- data.frame(a = 1, b = 2)

  testthat::expect_warning(
    test_fun(df, groups = c("a", "c"))
  )
})

testthat::test_that("validate_names() issues message when type='message'", {
  test_fun <- function(df, groups) {
    validate_names(input = df, check_names = groups, type = "message")
    df
  }

  df <- data.frame(a = 1, b = 2)

  testthat::expect_message(
    test_fun(df, groups = c("a", "c"))
  )
})

testthat::test_that("validate_names() handles large valid_set logic", {
  test_fun <- function(df, groups) {
    validate_names(input = df, check_names = groups, type = "error")
    df
  }

  df <- as.data.frame(matrix(ncol = 15, nrow = 1))
  colnames(df) <- paste0("col", 1:15)

  testthat::expect_error(
    test_fun(df, groups = c("col1", "badname")),
    regexp = "Some examples of valid column names"
  )
})

testthat::test_that("validate_names() respects null_ok", {
  test_fun <- function(df, groups, null_ok) {
    validate_names(input = df, check_names = groups, null_ok = null_ok)
    df
  }

  df <- data.frame(a = 1)

  testthat::expect_silent(
    test_fun(df, groups = NULL, null_ok = TRUE)
  )

  testthat::expect_error(
    test_fun(df, groups = NULL, null_ok = FALSE),
    regexp = "must not be NULL"
  )
})

testthat::test_that("validate_names() checks NA logic when na_ok=FALSE", {
  test_fun <- function(df, groups) {
    validate_names(input = df, check_names = groups, na_ok = FALSE)
    df
  }

  df <- data.frame(a = 1)

  testthat::expect_error(
    test_fun(df, groups = c(NA, "a")),
    regexp = "contains invalid column names such as"
  )
})

testthat::test_that("validate_names() validates that input is a data frame or tibble", {
  test_fun <- function(x) {
    validate_names(input = x, check_names = "a")
    x
  }

  bad_input <- list(a = 1)

  testthat::expect_error(
    test_fun(bad_input),
    regexp = "must be of class"
  )
})

testthat::test_that("validate_names() validates check_names is character", {
  test_fun <- function(df, groups) {
    validate_names(input = df, check_names = groups)
    df
  }

  df <- data.frame(a = 1)

  testthat::expect_error(
    test_fun(df, groups = 1),
    regexp = "must be of class.*character.*factor"
  )
})

testthat::test_that("validate_names() handles var_name override", {
  test_fun <- function(df, groups) {
    validate_names(
      input = df,
      check_names = groups,
      var_name = "my_cols",
      type = "error"
    )
    df
  }

  df <- data.frame(a = 1)

  testthat::expect_error(
    test_fun(df, groups = c("bad")),
    regexp = "my_cols"
  )
})
