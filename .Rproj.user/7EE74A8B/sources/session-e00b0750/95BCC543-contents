testthat::test_that("respiratory_02 produces expected results", {

  # Synthetic test data
  test_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    emedications_03 = c("Oxygen", "Oxygen", "Oxygen", "Oxygen", "Oxygen"),
    evitals_12 = c(60, 59, 58, 57, 56),
    eprocedures_03 = rep("applicable thing", 5)
  )

  # Run function
  result <- suppressWarnings(respiratory_02(
    df = test_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    emedications_03_col = emedications_03,
    evitals_12_col = evitals_12,
    eprocedures_03_col = eprocedures_03,
    confidence_interval = TRUE
  ))

  expect_s3_class(result, "data.frame")
  expect_true(all(c("pop", "numerator", "denominator", "prop", "prop_label", "lower_ci", "upper_ci") %in% names(result)))

  # should throw a warning due to small counts
  testthat::expect_warning(respiratory_02(
    df = test_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    emedications_03_col = emedications_03,
    evitals_12_col = evitals_12,
    eprocedures_03_col = eprocedures_03,
    confidence_interval = TRUE
  ))

  # Run function
  result <- respiratory_02(
    df = test_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    emedications_03_col = emedications_03,
    evitals_12_col = evitals_12,
    eprocedures_03_col = eprocedures_03
  )

  # Check structure
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(all(c("measure", "pop", "numerator", "denominator", "prop", "prop_label") %in% names(result)))

  # Check calculations
  testthat::expect_equal(sum(result$numerator), 10)
  testthat::expect_equal(sum(result$denominator), 10)
  testthat::expect_equal(result$prop[result$pop == "All"], 1)
  testthat::expect_equal(nrow(result), 3)

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

  medications_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    emedications_03 = c("Oxygen", "Oxygen", "Oxygen", "Oxygen", "Oxygen")

  )

  vitals_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    evitals_12 = c(60, 59, 58, 57, 56),

  )

  procedures_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    eprocedures_03 = rep("applicable thing", 5)

  )

  # test the success of the function

  result_2 <- respiratory_02(patient_scene_table = patient_table,
                              response_table = response_table,
                              medications_table = medications_table,
                              vitals_table = vitals_table,
                              procedures_table = procedures_table,
                              erecord_01_col = erecord_01,
                              incident_date_col = incident_date,
                              patient_DOB_col = patient_dob,
                              epatient_15_col = epatient_15,
                              epatient_16_col = epatient_16,
                              eresponse_05_col = eresponse_05,
                              emedications_03_col = emedications_03,
                              evitals_12_col = evitals_12,
                              eprocedures_03_col = eprocedures_03
                             )

  # Check calculations
  testthat::expect_equal(sum(result_2$numerator), 10)
  testthat::expect_equal(sum(result_2$denominator), 10)
  testthat::expect_equal(result_2$prop[result_2$pop == "All"], 1)
  testthat::expect_equal(nrow(result_2), 3)


})

testthat::test_that("respiratory_02 handles missing data correctly", {

  # Synthetic test data
  missing_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    emedications_03 = c("Oxygen", "Oxygen", NA_character_, "Oxygen", "Oxygen"),
    evitals_12 = c(60, 59, 58, 57, NA_integer_),
    eprocedures_03 = rep(NA_character_, 5)
  )

  result <- respiratory_02(
    df = missing_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    evitals_12_col = evitals_12,
    eprocedures_03_col = eprocedures_03
  )

  testthat::expect_true(nrow(result) > 0)
  testthat::expect_true(all(!is.na(result$denominator)))

})

testthat::test_that("respiratory_02 returns empty result for non-matching criteria", {

  # Synthetic test data
  non_matching_data <- tibble::tibble(
    erecord_01 = c("R1", "R2"),
    epatient_15 = c(30, 50),
    epatient_16 = c("Years", "Years"),
    eresponse_05 = c("Non-911 Call", "Non-911 Call"),
    emedications_03 = c("None", "None"),
    evitals_12 = c(60, 59),
    eprocedures_03 = rep("710925007", 2)
  )

  result <- respiratory_02(
    df = non_matching_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    emedications_03_col = emedications_03,
    evitals_12_col = evitals_12,
    eprocedures_03_col = eprocedures_03
  )

  testthat::expect_equal(sum(result$denominator), 0)
})

