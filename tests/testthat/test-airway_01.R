testthat::test_that("airway_01 requires either df or all required tables", {
  testthat::expect_no_success(airway_01())

  testthat::expect_error(airway_01(df = tibble::tibble()))

  testthat::expect_error(airway_01(patient_scene_table = tibble::tibble(),
                         response_table = tibble::tibble(),
                         arrest_table = tibble::tibble(),
                         procedures_table = tibble::tibble(),
                         vitals_table = tibble::tibble()))
})

testthat::test_that("airway_01 detects invalid parameter types", {
  testthat::expect_error(airway_01(df = list()))

  testthat::expect_error(airway_01(patient_scene_table = list(),
                         response_table = tibble::tibble(),
                         arrest_table = tibble::tibble(),
                         procedures_table = tibble::tibble(),
                         vitals_table = tibble::tibble()))

  testthat::expect_error(airway_01(df = tibble::tibble(),
                         erecord_01_col = 123))

  testthat::expect_error(airway_01(df = tibble::tibble(),
                         epatient_16_col = TRUE))
})

testthat::test_that("airway_01 validates required column names", {
  sample_data <- tibble::tibble(erecord_01 = 1:5, epatient_15 = c(30, 40, 50, 20, 10))

  testthat::expect_error(airway_01(df = sample_data,
                         erecord_01_col = "invalid_column",
                         epatient_15_col = "epatient_15",
                         epatient_16_col = "epatient_16"))

  testthat::expect_error(airway_01(df = sample_data,
                         erecord_01_col = "erecord_01",
                         epatient_15_col = "epatient_15",
                         epatient_16_col = "epatient_16"))
})

testthat::test_that("airway_01 handles empty datasets gracefully", {
  empty_df <- tibble::tibble()

  testthat::expect_error(airway_01(df = empty_df,
                         erecord_01_col = "erecord_01",
                         epatient_15_col = "epatient_15",
                         epatient_16_col = "epatient_16"))
})
