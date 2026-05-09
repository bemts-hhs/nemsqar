testthat::test_that("validate_set() accepts valid values", {
  testthat::expect_no_error(
    validate_set(input = c("a", "b"), valid_set = c("a", "b", "c"))
  )
})

testthat::test_that("validate_set() errors on invalid values (default type = error)", {
  testthat::expect_error(
    validate_set(input = c("a", "z"), valid_set = c("a", "b", "c"))
  )
})

testthat::test_that("validate_set() warns on invalid values when type = 'warning'", {
  testthat::expect_warning(
    validate_set(
      input = c("a", "z"),
      valid_set = c("a", "b", "c"),
      type = "warning"
    )
  )
})

testthat::test_that("validate_set() messages on invalid values when type = 'message'", {
  testthat::expect_message(
    validate_set(
      input = c("a", "z"),
      valid_set = c("a", "b", "c"),
      type = "message"
    )
  )
})

testthat::test_that("validate_set() handles NA correctly", {
  # NA allowed
  testthat::expect_no_error(
    validate_set(input = c("a", NA), valid_set = c("a", NA), na_ok = TRUE)
  )

  # NA not allowed
  testthat::expect_error(
    validate_set(input = c("a", NA), valid_set = c("a", "b"), na_ok = FALSE)
  )
})

testthat::test_that("validate_set() handles NULL correctly", {
  # NULL allowed
  testthat::expect_no_error(
    validate_set(input = NULL, valid_set = c("a", "b"), null_ok = TRUE)
  )

  # NULL not allowed
  testthat::expect_error(
    validate_set(input = NULL, valid_set = c("a", "b"), null_ok = FALSE)
  )
})

testthat::test_that("validate_set() respects var_name override", {
  testthat::expect_error(
    validate_set(
      input = c("x", "y"),
      valid_set = c("a", "b"),
      var_name = "CustomVar",
      type = "error"
    )
  )
})

testthat::test_that("validate_set() rejects invalid type argument", {
  testthat::expect_error(
    validate_set(input = "a", valid_set = c("a", "b"), type = "invalid_type")
  )
})

testthat::test_that("validate_set() handles small valid_set formatting branch", {
  small_valid <- letters[1:3] # length <= 10

  testthat::expect_error(
    validate_set(input = c("x"), valid_set = small_valid)
  )
})

testthat::test_that("validate_set() handles large valid_set formatting branch", {
  large_valid <- as.character(seq_len(50)) # length > 10

  testthat::expect_error(
    validate_set(input = c("1000"), valid_set = large_valid)
  )
})

testthat::test_that("validate_set() correctly identifies all invalid values", {
  testthat::expect_error(
    validate_set(input = c("a", "b", "c", "zzz"), valid_set = c("a", "b"))
  )
})

testthat::test_that("validate_set() correctly handles multi-value invalid input", {
  testthat::expect_error(
    validate_set(input = c("x", "y", "z"), valid_set = c("a", "b", "c"))
  )
})
