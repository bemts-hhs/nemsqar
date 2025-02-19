testthat::test_that("tbi_01 produces expected results", {

  # Synthetic test data
  test_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    esituation_11 = c(rep("S02", 3), rep("S06", 2)),
    esituation_12 = c(rep("S09.90", 2), rep("S06.0X9", 3)),
    evitals_06 = c(85, 80, 100, 90, 82),
    evitals_12 = c(95, 96, 97, 98, 99),
    evitals_16 = c(35, 36, 37, 38, 39),
    evitals_23 = rep(8, 5),
    evitals_26 = c("Verbal", "Painful", "Unresponsive", "Verbal", "Painful"),
    edisposition_30 = c(4230001, 4230003, 4230001, 4230007, 4230007)
  )

  # Run function
  result <- tbi_01(
    df = test_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    esituation_11_col = esituation_11,
    esituation_12_col = esituation_12,
    evitals_06_col = evitals_06,
    evitals_12_col = evitals_12,
    evitals_16_col = evitals_16,
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
    esituation_11 = c(rep("S02", 3), rep("S06", 2)),
    esituation_12 = c(rep("S09.90", 2), rep("S06.0X9", 3)),
  )


  vitals_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    evitals_06 = c(85, 80, 100, 90, 82),
    evitals_12 = c(95, 96, 97, 98, 99),
    evitals_16 = c(35, 36, 37, 38, 39),
    evitals_23 = rep(8, 5),
    evitals_26 = c("Verbal", "Painful", "Unresponsive", "Verbal", "Painful")
  )

  disposition_table <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    edisposition_30 = c(4230001, 4230003, 4230001, 4230007, 4230007)
  )


  # test the success of the function

  result_2 <- tbi_01(patient_scene_table = patient_table,
                         response_table = response_table,
                         situation_table = situation_table,
                         vitals_table = vitals_table,
                         disposition_table = disposition_table,
                         erecord_01_col = erecord_01,
                         epatient_15_col = epatient_15,
                         epatient_16_col = epatient_16,
                         eresponse_05_col = eresponse_05,
                         esituation_11_col = esituation_11,
                         esituation_12_col = esituation_12,
                         evitals_06_col = evitals_06,
                         evitals_12_col = evitals_12,
                         evitals_16_col = evitals_16,
                         evitals_23_col = evitals_23,
                         evitals_26_col = evitals_26,
                         transport_disposition_col = edisposition_30
                     )

  # Check calculations
  testthat::expect_equal(sum(result_2$numerator), 5)
  testthat::expect_equal(sum(result_2$denominator), 5)
  testthat::expect_equal(result_2$prop[result_2$pop == "Adult"], 1)
  testthat::expect_equal(nrow(result_2), 2)


})

testthat::test_that("tbi_01 handles missing data correctly", {

  # Synthetic test data
  missing_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    esituation_11 = c(rep("S02", 3), rep("S06", 2)),
    esituation_12 = c(rep("S09.90", 2), rep("S06.0X9", 3)),
    evitals_06 = c(NA_integer_, 80, 100, 90, 82),
    evitals_12 = c(95, NA_integer_, 97, 98, 99),
    evitals_16 = c(35, 36, 37, NA_integer_, 39),
    evitals_23 = rep(8, 5),
    evitals_26 = c("Verbal", "Painful", NA_character_, "Verbal", "Painful"),
    edisposition_30 = c(4230001, 4230003, NA_integer_, 4230007, 4230007)
  )

  result <- tbi_01(
    df = missing_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    esituation_11_col = esituation_11,
    esituation_12_col = esituation_12,
    evitals_06_col = evitals_06,
    evitals_12_col = evitals_12,
    evitals_16_col = evitals_16,
    evitals_23_col = evitals_23,
    evitals_26_col = evitals_26,
    transport_disposition_col = edisposition_30
  )

  testthat::expect_true(nrow(result) > 0)
  testthat::expect_true(all(!is.na(result$denominator)))

})

testthat::test_that("tbi_01 returns empty result for non-matching criteria", {

  # Synthetic test data
  non_matching_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    esituation_11 = c(rep("S02", 3), rep("S06", 2)),
    esituation_12 = c(rep("S09.90", 2), rep("S06.0X9", 3)),
    evitals_06 = c(85, 80, 100, 90, 82),
    evitals_12 = c(95, 96, 97, 98, 99),
    evitals_16 = c(35, 36, 37, 38, 39),
    evitals_23 = rep(8, 5),
    evitals_26 = c("Verbal", "Painful", "Unresponsive", "Verbal", "Painful"),
    edisposition_30 = rep("not a transport", 5)
  )

  result <- tbi_01(
    df = non_matching_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    esituation_11_col = esituation_11,
    esituation_12_col = esituation_12,
    evitals_06_col = evitals_06,
    evitals_12_col = evitals_12,
    evitals_16_col = evitals_16,
    evitals_23_col = evitals_23,
    evitals_26_col = evitals_26,
    transport_disposition_col = edisposition_30
  )

  testthat::expect_equal(sum(result$denominator), 0)
})

