testthat::test_that("compare_formals() correctly identifies shared and unique arguments", {
  f1 <- function(a, b, c) {}
  f2 <- function(b, c, d) {}
  f3 <- function(c, e) {}

  res <- compare_formals(fun = f1, others = list(f2, f3))

  testthat::expect_setequal(res$shared, c("b", "c"))
  testthat::expect_setequal(res$unique, "a")
})

testthat::test_that("compare_formals() handles when others has no overlapping arguments", {
  f1 <- function(a, b) {}
  f2 <- function(x, y) {}

  res <- compare_formals(fun = f1, others = list(f2))

  testthat::expect_length(res$shared, 0)
  testthat::expect_setequal(res$unique, c("a", "b"))
})

testthat::test_that("compare_formals() handles empty others list", {
  f1 <- function(a, b, c) {}

  res <- compare_formals(fun = f1, others = list())

  testthat::expect_length(res$shared, 0)
  testthat::expect_setequal(res$unique, c("a", "b", "c"))
})

testthat::test_that("compare_formals() handles functions with no arguments", {
  f1 <- function() {}
  f2 <- function(a, b) {}

  res <- compare_formals(fun = f1, others = list(f2))

  testthat::expect_length(res$shared, 0)
  testthat::expect_length(res$unique, 0)
})

testthat::test_that("compare_formals() handles ellipsis correctly", {
  f1 <- function(a, b, ...) {}
  f2 <- function(x, ..., y) {}

  res <- compare_formals(fun = f1, others = list(f2))

  testthat::expect_true("..." %in% res$shared)
  testthat::expect_true("a" %in% res$unique)
  testthat::expect_true("b" %in% res$unique)
})

testthat::test_that("compare_formals() ignores order of formals", {
  f1 <- function(a, b, c) {}
  f2 <- function(c, a, d) {}

  res <- compare_formals(fun = f1, others = list(f2))

  testthat::expect_setequal(res$shared, c("a", "c"))
  testthat::expect_setequal(res$unique, "b")
})

testthat::test_that("compare_formals() works when used inside another function", {
  wrapper <- function() {
    f1 <- function(x, y) {}
    f2 <- function(y, z) {}
    compare_formals(fun = f1, others = list(f2))
  }

  res <- wrapper()

  testthat::expect_setequal(res$shared, "y")
  testthat::expect_setequal(res$unique, "x")
})

testthat::test_that("compare_formals() handles mixed list of functions in others", {
  f1 <- function(a, b, c) {}
  f2 <- function(b) {}
  f3 <- function(c, d) {}
  f4 <- function(z) {}

  res <- compare_formals(fun = f1, others = list(f2, f3, f4))

  testthat::expect_setequal(res$shared, c("b", "c"))
  testthat::expect_setequal(res$unique, "a")
})
