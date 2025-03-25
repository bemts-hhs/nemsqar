###_____________________________________________________________________________
# test the rmm function
###_____________________________________________________________________________

testthat::test_that("rmm function validates inputs correctly", {
  # Test if data is a data frame
  testthat::expect_error(rmm(data = NULL, Ps_col = Ps, outcome_col = survival))

  set.seed(01232025)

  # Test missing Ps_col
  df <- tibble::tibble(Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)), survival = rbinom(1000, 1, prob = 0.9))
  testthat::expect_error(rmm(data = df, outcome_col = survival, n_samples = 5))

  # Test missing outcome_col
  testthat::expect_error(rmm(data = df, Ps_col = Ps))

  # Test non-binary outcome_col
  df_non_binary <- tibble::tibble(Ps = plogis(rnorm(5, mean = 2, sd = 1.5)), survival = c(1, 2, 3, 4, 5))
  testthat::expect_error(rmm(data = df_non_binary, Ps_col = Ps, outcome_col = survival, n_samples = 5))

  # Test non-numeric Ps_col
  df_non_numeric <- tibble::tibble(Ps = c("a", "b", "c"), survival = c(1, 0, 1))
  testthat::expect_error(rmm(data = df_non_numeric, Ps_col = Ps, outcome_col = survival, n_samples = 5))

  # Test Ps values > 1
  df_ps_above_1 <- tibble::tibble(Ps = sample(30:150, size = 1000, replace = TRUE), survival = sample(c(1,0), size = 1000, replace = TRUE))
  testthat::expect_error(rmm(data = df_ps_above_1, Ps_col = Ps, outcome_col = survival, n_samples = 5))

  #Test incorrect input to n_samples
  testthat::expect_error(rmm(data = data.frame(Ps = c(0.1, 0.5, 0.9, .005), survival = c(1, 0, 1, 0)),
                   Ps_col = Ps, outcome_col = survival,
                   n_samples = "1000")
               )
})

testthat::test_that("rmm function computes binning correctly", {

  set.seed(01232025)

  # Test the binning process with some mock data
  df <- tibble::tibble(Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)), survival = rbinom(1000, 1, prob = 0.9))

  # Check if binning works as expected
  result <- rmm(data = df, Ps_col = Ps, outcome_col = survival, Divisor1 = 5, Divisor2 = 5, n_samples = 5)
  testthat::expect_true(all(c("population_RMM_LL", "population_RMM", "population_RMM_UL", "population_CI", "bootstrap_RMM_LL", "bootstrap_RMM", "bootstrap_RMM_UL", "bootstrap_CI") %in% colnames(result)))

  # Test that RMM is calculated
  testthat::expect_true(all(!is.na(result$population_RMM)))
  testthat::expect_true(all(!is.na(result$bootstrap_RMM)))
})

testthat::test_that("rmm function calculates RMM and its confidence intervals", {

  set.seed(01232025)

  # Test with mock data for RMM calculation
  df <- tibble::tibble(Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)), survival = rbinom(1000, 1, prob = 0.9))

  result <- rmm(data = df, Ps_col = Ps, outcome_col = survival, Divisor1 = 5, Divisor2 = 5, n_samples = 5)

  # Test for upper and lower bounds of RMM
  testthat::expect_true(all(result$population_RMM_LL <= result$population_RMM))
  testthat::expect_true(all(result$population_RMM_UL >= result$population_RMM))
  testthat::expect_true(all(result$bootstrap_RMM_LL <= result$bootstrap_RMM))
  testthat::expect_true(all(result$bootstrap_RMM_UL >= result$bootstrap_RMM))
})

testthat::test_that("rmm function handles the pivot argument correctly", {
  # Test with pivot = TRUE
  df <- tibble::tibble(Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)), survival = rbinom(1000, 1, prob = 0.9))

  result_pivot <- rmm(data = df, Ps_col = Ps, outcome_col = survival, pivot = TRUE, n_samples = 5)
  testthat::expect_true("stat" %in% colnames(result_pivot))
  testthat::expect_true("value" %in% colnames(result_pivot))

  # Test without pivot (default is FALSE)
  result_no_pivot <- rmm(data = df, Ps_col = Ps, outcome_col = survival, pivot = FALSE, n_samples = 5)
  testthat::expect_false("stat" %in% colnames(result_no_pivot))
  testthat::expect_false("value" %in% colnames(result_no_pivot))
})

testthat::test_that("rmm function handles edge cases correctly", {

  # Test with edge case: only one row
  df_one_row <- tibble::tibble(Ps = 0.50, survival = 1)
  testthat::expect_error(rmm(data = df_one_row, Ps_col = Ps, outcome_col = survival))

  # Test with all NA values in Ps
  df_na_ps <- tibble::tibble(Ps = c(NA, NA, NA), survival = c(1, 0, 1))
  testthat::expect_error(rmm(data = df_na_ps, Ps_col = Ps, outcome_col = survival))

  # Test with all missing outcome values
  df_na_outcome <- tibble::tibble(Ps = c(.20, .50, .80), survival = c(NA, NA, NA))
  testthat::expect_error(rmm(data = df_na_outcome, Ps_col = Ps, outcome_col = survival))

})

testthat::test_that("rmm function handles the seed argument correctly", {

  # Test with a fixed seed value to ensure reproducibility
  set.seed(123)
  df <- tibble::tibble(Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)), survival = rbinom(1000, 1, prob = 0.9))

  result_1 <- rmm(data = df, Ps_col = Ps, outcome_col = survival, n_samples = 5, seed = 123)
  result_2 <- rmm(data = df, Ps_col = Ps, outcome_col = survival, n_samples = 5, seed = 123)

  # Ensure the results are identical with the same seed
  testthat::expect_identical(result_1, result_2)

  # Test with a different seed to check if results change
  result_3 <- rmm(data = df, Ps_col = Ps, outcome_col = survival, n_samples = 5, seed = 456)

  testthat::expect_false(identical(result_1, result_3))
})

testthat::test_that("rmm function handles the group_vars argument correctly", {

  set.seed(01232025)

  # Test with grouping by a variable (e.g., a factor)
  df <- tibble::tibble(Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)),
                       survival = rbinom(1000, 1, prob = 0.9),
                       group = rep(letters[1:5], each = 200))

  result_grouped <- rmm(data = df, Ps_col = Ps, outcome_col = survival, group_vars = "group", n_samples = 100)

  # Test if the output contains the group variable
  testthat::expect_true("group" %in% colnames(result_grouped))

  # Ensure that results are calculated per group
  group_means <- result_grouped %>% dplyr::group_by(group) %>% dplyr::summarize(mean_rmm = mean(population_RMM, na.rm = TRUE))
  testthat::expect_true(all(!is.na(group_means$mean_rmm)))
})

testthat::test_that("rmm function handles edge cases with seed and group_vars", {

  # Test with missing group_vars
  df_missing_group <- tibble::tibble(Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)), survival = rbinom(1000, 1, prob = 0.9))
  result_no_group <- rmm(data = df_missing_group, Ps_col = Ps, outcome_col = survival, n_samples = 5, seed = 123)

  # Test that no error occurs and RMM is calculated
  testthat::expect_true(all(!is.na(result_no_group$population_RMM)))

  # Test with group_vars but no valid grouping
  df_invalid_group <- tibble::tibble(Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)),
                                     survival = rbinom(1000, 1, prob = 0.9),
                                     group = rep(NA, 1000))

  testthat::expect_error(rmm(data = df_invalid_group, Ps_col = Ps, outcome_col = survival, group_vars = group, n_samples = 5, seed = 123))
})

testthat::test_that("rmm function handles missing values with seed and group_vars correctly", {

  # Create a larger dataset with missing values in Ps, survival, and group variables
  set.seed(123)
  df_missing_values <- tibble::tibble(
    Ps = c(rep(NA, 200), plogis(rnorm(800, mean = 2, sd = 1.5))),  # 200 NAs in Ps
    survival = c(rep(NA, 200), rbinom(800, 1, prob = 0.8)),           # 200 NAs in survival
    group = rep(c("A", "B", "C", "D", "E"), each = 200)               # Group variable
  )

  # Expect warning when running rmm with missing values in the dataset
  testthat::expect_warning(
    rmm(data = df_missing_values, Ps_col = Ps, outcome_col = survival, group_vars = "group", n_samples = 5, seed = 123)
  )

})

testthat::test_that("rmm function handles large datasets without performance issues", {

  # Test with a larger dataset to assess performance
  df_large <- tibble::tibble(Ps = plogis(rnorm(100000, mean = 2, sd = 1.5)),
                             survival = rbinom(100000, 1, prob = 0.9))

  # record time
  begin <- Sys.time()

  result_large <- rmm(data = df_large, Ps_col = Ps, outcome_col = survival, n_samples = 1000)

  # record time
  end <- Sys.time()

  # get time difference
  difference <- as.numeric(difftime(end, begin, units = "secs"))

  # Ensure the function runs without error and produces results
  testthat::expect_true(all(!is.na(result_large$population_RMM)))

  # Check for a good runtime
  # less than 1 minute
  testthat::expect_lt(difference, 60)

})

###_____________________________________________________________________________
# test the rm_bin_summary function
###_____________________________________________________________________________

testthat::test_that("rm_bin_summary validates inputs correctly", {
  # Test if data is a data frame
  testthat::expect_error(rm_bin_summary(data = NULL, Ps_col = Ps, outcome_col = survival))

  set.seed(01232025)

  # Test missing Ps_col
  df <- tibble::tibble(Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)), survival = rbinom(1000, 1, prob = 0.9))
  testthat::expect_error(rm_bin_summary(data = df, outcome_col = survival))

  # Test missing outcome_col
  testthat::expect_error(rm_bin_summary(data = df, Ps_col = Ps))

  # Test non-binary outcome_col
  df_non_binary <- tibble::tibble(Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)), survival = sample(1:3, 1000, replace = TRUE))
  testthat::expect_error(rm_bin_summary(data = df_non_binary, Ps_col = Ps, outcome_col = survival))

  # Test non-numeric Ps_col
  df_non_numeric <- tibble::tibble(Ps = c("a", "b", "c"), survival = c(1, 0, 1))
  testthat::expect_error(rm_bin_summary(data = df_non_numeric, Ps_col = Ps, outcome_col = survival))

  # Test Ps values > 1
  df_ps_above_1 <- tibble::tibble(Ps = c(150, 80, 30), survival = c(1, 0, 1))
  testthat::expect_error(rm_bin_summary(data = df_ps_above_1, Ps_col = Ps, outcome_col = survival))
})

testthat::test_that("nonlinear_bins produces correct bin data", {

  set.seed(01232025)

  # Test missing Ps_col
  df <- tibble::tibble(Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)), survival = rbinom(1000, 1, prob = 0.9))

  bin_data <- nonlinear_bins(
    df,
    Ps_col = Ps,
    outcome_col = survival,
    divisor1 = 5,
    divisor2 = 5,
    threshold_1 = 0.9,
    threshold_2 = 0.99
  )

  testthat::expect_true("bin_stats" %in% names(bin_data))
  testthat::expect_true("intervals" %in% names(bin_data))
  testthat::expect_equal(nrow(bin_data$bin_stats), 10)  # assuming 5 bins
})

testthat::test_that("bootstrap data has the correct number of samples", {

  set.seed(01232025)

  # Test missing Ps_col
  df <- tibble::tibble(Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)), survival = rbinom(1000, 1, prob = 0.9))

  bootstrap_data <- df |>
    infer::generate(reps = 100, type = "bootstrap")

  testthat::expect_equal(nrow(bootstrap_data), 100 * nrow(df))
})

testthat::test_that("bin statistics are calculated correctly", {

  set.seed(01232025)

  # Test missing Ps_col
  df <- tibble::tibble(Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)), survival = rbinom(1000, 1, prob = 0.9))

  try_function <- rm_bin_summary(df, Ps, survival, n_samples = 5)

  testthat::expect_true("TA_b" %in% names(try_function))
  testthat::expect_true("TD_b" %in% names(try_function))
  testthat::expect_true("EM_b" %in% names(try_function))
})

testthat::test_that("RMM is calculated correctly", {

  set.seed(01232025)

  # Test missing Ps_col
  df <- tibble::tibble(Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)), survival = rbinom(1000, 1, prob = 0.9))

  rmm_result <- df |>
    rm_bin_summary(Ps, survival, n_samples = 5)

  testthat::expect_true("population_RMM" %in% names(rmm_result))
  testthat::expect_true(all(rmm_result$population_RMM >= -1) || all(rmm_result$population_RMM <= 1))

})

testthat::test_that("confidence intervals are correctly computed in final RMM", {

  set.seed(10232015)

  # Test missing Ps_col
  df <- tibble::tibble(Ps = plogis(rnorm(10000, mean = 2, sd = 1.5)), survival = rbinom(10000, 1, prob = 0.9))

  rmm_result <- rm_bin_summary(df, Ps, survival, n_samples = 100)

  testthat::expect_true("bootstrap_RMM_LL" %in% names(rmm_result))
  testthat::expect_true("bootstrap_RMM_UL" %in% names(rmm_result))
  testthat::expect_true("bootstrap_CI" %in% names(rmm_result))
  testthat::expect_true(all(rmm_result$population_RMM_UL > rmm_result$population_RMM_LL))  # CI upper should be greater than lower
})

testthat::test_that("RMM final data is correctly sorted by bin_number", {

  set.seed(01232025)

  # Test missing Ps_col
  df <- tibble::tibble(Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)), survival = rbinom(1000, 1, prob = 0.9))

  rmm_result_final <- rm_bin_summary(df, Ps, survival, n_samples = 5)

  testthat::expect_equal(min(rmm_result_final$bin_number), 1)
  testthat::expect_equal(max(rmm_result_final$bin_number), 10)  # Assuming 10 bins

})

testthat::test_that("rm_bin_summary validates group_vars correctly", {
  set.seed(01232025)

  # Test valid grouping
  df <- tibble::tibble(Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)),
                       survival = rbinom(1000, 1, prob = 0.9),
                       group = sample(letters[1:5], 1000, replace = TRUE))

  grouped_result <- rm_bin_summary(df, Ps, survival, group_vars = "group", n_samples = 5)
  testthat::expect_true("group" %in% names(grouped_result))
  testthat::expect_true(all(grouped_result$group %in% unique(df$group)))

  # Test invalid group_vars (non-existent column)
  testthat::expect_error(rm_bin_summary(df, Ps, survival, group_vars = non_existent_col))
})

testthat::test_that("rm_bin_summary handles the seed argument correctly", {
  # Test consistency with seed
  df <- tibble::tibble(Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)),
                       survival = rbinom(1000, 1, prob = 0.9))

  result1 <- rm_bin_summary(df, Ps, survival, n_samples = 5, seed = 12345)
  result2 <- rm_bin_summary(df, Ps, survival, n_samples = 5, seed = 12345)

  # Ensure the results are the same for the same seed
  testthat::expect_equal(result1, result2)

  # Test different seed values result in different outputs
  result3 <- rm_bin_summary(df, Ps, survival, n_samples = 5, seed = 67890)
  testthat::expect_false(identical(result1, result3))
})

testthat::test_that("rm_bin_summary handles extreme edge cases for Ps values", {
  # Test edge cases for Ps column (extremely low/high values)
  df_extreme <- tibble::tibble(Ps = c(1e-10, 1, 1e+10), survival = c(1, 0, 1))

  # Ensure that error is thrown with values above 1 or less than 0
  testthat::expect_error(rm_bin_summary(df_extreme, Ps, survival, n_samples = 5))

})

testthat::test_that("rm_bin_summary produces error with invalid n_samples", {
  set.seed(01232025)

  df <- tibble::tibble(Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)),
                       survival = rbinom(1000, 1, prob = 0.9))

  # Invalid n_samples (non-integer, negative)
  testthat::expect_error(rm_bin_summary(df, Ps, survival, n_samples = -1))
  testthat::expect_error(rm_bin_summary(df, Ps, survival, n_samples = 3.5))
})

testthat::test_that("rm_bin_summary with missing group_vars argument", {
  set.seed(01232025)

  df <- tibble::tibble(Ps = plogis(rnorm(1000, mean = 2, sd = 1.5)),
                       survival = rbinom(1000, 1, prob = 0.9))

  # Ensure no error when group_vars is not supplied
  no_group_result <- rm_bin_summary(df, Ps, survival, n_samples = 5)
  testthat::expect_true("bin_number" %in% names(no_group_result))
  testthat::expect_true(nrow(no_group_result) > 0)
})
