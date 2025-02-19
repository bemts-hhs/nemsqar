testthat::test_that("syncope_01 produces expected results", {

  # Synthetic test data
  test_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    esituation_09 = c(rep("R55", 3), rep("R40.4", 2)),
    esituation_10 = c(rep("R40.4", 2), rep("R55", 3)),
    esituation_11 = c(rep("R55", 3), rep("R40.4", 2)),
    esituation_12 = c(rep("R40.4", 2), rep("R55", 3)),
    evitals_04 = rep("15 Lead", 5)
  )

  # Run function
  result <- syncope_01(
    df = test_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    esituation_09_col = esituation_09,
    esituation_10_col = esituation_10,
    esituation_11_col = esituation_11,
    esituation_12_col = esituation_12,
    evitals_04_col = evitals_04
  )

  # Check structure
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(all(c("measure", "pop", "numerator", "denominator", "prop", "prop_label") %in% names(result)))

  # Check calculations
  testthat::expect_equal(sum(result$numerator), 4)
  testthat::expect_equal(sum(result$denominator), 4)
  testthat::expect_equal(result$prop[result$pop == "Adult"], 1)
  testthat::expect_equal(nrow(result), 2)

  # create tables to test correct functioning
  patient_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01", "2025-01-01", "2025-06-01")),
    patient_dob = as.Date(c("2000-01-01", "2020-01-01", "2023-02-01", "2023-01-01", "1970-06-01")),
    epatient_15 = c(25, 5, 2, 2, 55),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Years", "Years")

  )

  response_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    eresponse_05 = rep(2205001, 5)

  )

  situation_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    esituation_09 = c(rep("R55", 3), rep("R40.4", 2)),
    esituation_10 = c(rep("R40.4", 2), rep("R55", 3)),
    esituation_11 = c(rep("R55", 3), rep("R40.4", 2)),
    esituation_12 = c(rep("R40.4", 2), rep("R55", 3)),
  )


  vitals_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    evitals_04 = rep("15 Lead", 5)

  )


  # test the success of the function

  result_2 <- syncope_01(patient_scene_table = patient_table,
                         response_table = response_table,
                         situation_table = situation_table,
                         vitals_table = vitals_table,
                         erecord_01_col = erecord_01,
                         epatient_15_col = epatient_15,
                         epatient_16_col = epatient_16,
                         eresponse_05_col = eresponse_05,
                         esituation_09_col = esituation_09,
                         esituation_10_col = esituation_10,
                         esituation_11_col = esituation_11,
                         esituation_12_col = esituation_12,
                         evitals_04_col = evitals_04
                         )

  # Check calculations
  testthat::expect_equal(sum(result_2$numerator), 5)
  testthat::expect_equal(sum(result_2$denominator), 5)
  testthat::expect_equal(result_2$prop[result_2$pop == "Adult"], 1)
  testthat::expect_equal(nrow(result_2), 2)


})

testthat::test_that("syncope_01 handles missing data correctly", {

  # Synthetic test data
  missing_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    esituation_09 = c(rep("R55", 3), rep(NA_character_, 2)),
    esituation_10 = c(rep("R40.4", 2), rep("R55", 3)),
    esituation_11 = c(rep("R55", 3), rep(NA_character_, 2)),
    esituation_12 = c(rep(NA_character_, 2), rep("R55", 3)),
    evitals_04 = rep("15 Lead", 5)
  )

  result <- syncope_01(
    df = missing_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    esituation_09_col = esituation_09,
    esituation_10_col = esituation_10,
    esituation_11_col = esituation_11,
    esituation_12_col = esituation_12,
    evitals_04_col = evitals_04
  )

  testthat::expect_true(nrow(result) > 0)
  testthat::expect_true(all(!is.na(result$denominator)))

})

testthat::test_that("syncope_01 returns empty result for non-matching criteria", {

  # Synthetic test data
  non_matching_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    esituation_09 = c(rep("R", 3), rep("R", 2)),
    esituation_10 = c(rep("R", 2), rep("R", 3)),
    esituation_11 = c(rep("R", 3), rep("R", 2)),
    esituation_12 = c(rep("R", 2), rep("R", 3)),
    evitals_04 = rep("15 Lead", 5)
  )

  result <- syncope_01(
    df = non_matching_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    esituation_09_col = esituation_09,
    esituation_10_col = esituation_10,
    esituation_11_col = esituation_11,
    esituation_12_col = esituation_12,
    evitals_04_col = evitals_04
  )

  testthat::expect_equal(sum(result$denominator), 0)
})

