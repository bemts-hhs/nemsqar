testthat::test_that("ttr_01 produces expected results", {

  # Synthetic test data
  test_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    earrest_01 = rep("No", 5),
    evitals_06 = c(100, 90, 80, 70, 85),
    evitals_07 = c(80, 90, 50, 60, 87),
    evitals_10 = c(110, 89, 88, 71, 85),
    evitals_12 = c(50, 60, 70, 80, 75),
    evitals_14 = c(30, 9, 8, 7, 31),
    evitals_23 = c(6, 7, 8, 9, 10),
    evitals_26 = c(3326007, 3326005, 3326003, 3326001, 3326007),
    edisposition_30 = c(4230013, 4230009, 4230013, 4230009, 4230013)
  )

  # Run function
  result <- suppressWarnings(ttr_01(
    df = test_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    earrest_01_col = earrest_01,
    evitals_06_col = evitals_06,
    evitals_07_col = evitals_07,
    evitals_10_col = evitals_10,
    evitals_12_col = evitals_12,
    evitals_14_col = evitals_14,
    evitals_23_col = evitals_23,
    evitals_26_col = evitals_26,
    transport_disposition_col = edisposition_30,
    confidence_interval = TRUE
  ))

  # Check structure
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(all(c("measure", "pop", "numerator", "denominator", "prop", "prop_label", "lower_ci", "upper_ci") %in% names(result)))

  # should throw a warning due to small counts
  testthat::expect_warning(ttr_01(
    df = test_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    earrest_01_col = earrest_01,
    evitals_06_col = evitals_06,
    evitals_07_col = evitals_07,
    evitals_10_col = evitals_10,
    evitals_12_col = evitals_12,
    evitals_14_col = evitals_14,
    evitals_23_col = evitals_23,
    evitals_26_col = evitals_26,
    transport_disposition_col = edisposition_30,
    confidence_interval = TRUE
  ))

  # Run function with the first and last pain score columns
  result <- ttr_01(
    df = test_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    earrest_01_col = earrest_01,
    evitals_06_col = evitals_06,
    evitals_07_col = evitals_07,
    evitals_10_col = evitals_10,
    evitals_12_col = evitals_12,
    evitals_14_col = evitals_14,
    evitals_23_col = evitals_23,
    evitals_26_col = evitals_26,
    transport_disposition_col = edisposition_30
  )

  # Check structure
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(all(c("measure", "pop", "numerator", "denominator", "prop", "prop_label") %in% names(result)))

  # Check calculations
  testthat::expect_equal(sum(result$numerator), 5)
  testthat::expect_equal(sum(result$denominator), 5)
  testthat::expect_equal(result$prop[result$pop == "Adults"], 1)
  testthat::expect_equal(nrow(result), 2)

  # create tables to test correct functioning
  patient_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01", "2025-01-01", "2025-06-01")),
    patient_dob = as.Date(c("2000-01-01", "2020-01-01", "2023-02-01", "2023-01-01", "1970-06-01")),
    epatient_15 = c(25, 5, 2, 2, 55),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Years", "Years")

  )

  # response table
  response_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    eresponse_05 = rep(2205001, 5),
  )

  # arrest table
  arrest_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    earrest_01 = rep("No", 5)
  )

  # vitals table
  vitals_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    evitals_06 = c(100, 90, 80, 70, 85),
    evitals_07 = c(80, 90, 50, 60, 87),
    evitals_10 = c(110, 89, 88, 71, 85),
    evitals_12 = c(50, 60, 70, 80, 75),
    evitals_14 = c(30, 9, 8, 7, 31),
    evitals_23 = c(6, 7, 8, 9, 10),
    evitals_26 = c(3326007, 3326005, 3326003, 3326001, 3326007),
  )

  # disposition table
  disposition_table <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    edisposition_30 = c(4230013, 4230009, 4230013, 4230009, 4230013)
  )

  # test the success of the function
  result <- ttr_01(patient_scene_table = patient_table,
                        response_table = response_table,
                        arrest_table = arrest_table,
                        vitals_table = vitals_table,
                        disposition_table = disposition_table,
                        erecord_01_col = erecord_01,
                        incident_date_col = incident_date,
                        patient_DOB_col = patient_dob,
                        epatient_15_col = epatient_15,
                        epatient_16_col = epatient_16,
                        eresponse_05_col = eresponse_05,
                        earrest_01_col = earrest_01,
                        evitals_06_col = evitals_06,
                        evitals_07_col = evitals_07,
                        evitals_10_col = evitals_10,
                        evitals_12_col = evitals_12,
                        evitals_14_col = evitals_14,
                        evitals_23_col = evitals_23,
                        evitals_26_col = evitals_26,
                        transport_disposition_col = edisposition_30
                   )

  # Check calculations
  testthat::expect_equal(sum(result$numerator), 5)
  testthat::expect_equal(sum(result$denominator),5)
  testthat::expect_equal(result$prop[result$pop == "Adults"], 1)
  testthat::expect_equal(nrow(result), 2)

})

testthat::test_that("ttr_01 handles missing data correctly", {

  # Synthetic test data
  test_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    earrest_01 = rep("No", 5),
    evitals_06 = c(NA, 90, 80, 70, 85),
    evitals_07 = c(80, 90, 50, 60, 87),
    evitals_10 = c(110, 89, 88, 71, 85),
    evitals_12 = c(50, NA, 70, 80, 75),
    evitals_14 = c(30, 9, 8, 7, 31),
    evitals_23 = c(6, 7, 8, 9, NA),
    evitals_26 = c(3326007, 3326005, 3326003, NA, 3326007),
    edisposition_30 = c(4230013, 4230009, NA, 4230009, 4230013)
  )

  # run the function with first and last
  # pain scale columns
  result <- ttr_01(
    df = test_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    earrest_01_col = earrest_01,
    evitals_06_col = evitals_06,
    evitals_07_col = evitals_07,
    evitals_10_col = evitals_10,
    evitals_12_col = evitals_12,
    evitals_14_col = evitals_14,
    evitals_23_col = evitals_23,
    evitals_26_col = evitals_26,
    transport_disposition_col = edisposition_30
  )

  testthat::expect_true(nrow(result) > 0)
  testthat::expect_true(all(!is.na(result$denominator)))

})

testthat::test_that("ttr_01 returns empty result for non-matching criteria", {

  # Synthetic test data
  test_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    earrest_01 = rep("No", 5),
    evitals_06 = c(100, 90, 80, 70, 85),
    evitals_07 = c(80, 90, 50, 60, 87),
    evitals_10 = c(110, 89, 88, 71, 85),
    evitals_12 = c(50, 60, 70, 80, 75),
    evitals_14 = c(30, 9, 8, 7, 31),
    evitals_23 = c(6, 7, 8, 9, 10),
    evitals_26 = c(3326007, 3326005, 3326003, 3326001, 3326007),
    edisposition_30 = rep("a transport!", 5)
  )

  # run the function with the
  # initial and last pain scale
  # columns
  result <- ttr_01(
    df = test_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    earrest_01_col = earrest_01,
    evitals_06_col = evitals_06,
    evitals_07_col = evitals_07,
    evitals_10_col = evitals_10,
    evitals_12_col = evitals_12,
    evitals_14_col = evitals_14,
    evitals_23_col = evitals_23,
    evitals_26_col = evitals_26,
    transport_disposition_col = edisposition_30
  )

  testthat::expect_equal(sum(result$denominator), 0)

})

