testthat::test_that("stroke_01 produces expected results", {

  # Synthetic test data
  test_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    esituation_11 = c(rep("I60", 3), rep("I61", 2)),
    esituation_12 = c(rep("I63", 2), rep("I64", 3)),
    evitals_23 = c(16, 15, 14, 13, 12),
    evitals_26 = c("Alert", "Painful", "Verbal", "Unresponsive", "Alert"),
    evitals_29 = rep("positive", 5),
    evitals_30 = rep("a pain scale", 5)
  )

  # Run function
  result <- stroke_01(
    df = test_data,
    erecord_01_col = erecord_01,
    eresponse_05_col = eresponse_05,
    esituation_11_col = esituation_11,
    esituation_12_col = esituation_12,
    evitals_23_col = evitals_23,
    evitals_26_col = evitals_26,
    evitals_29_col = evitals_29,
    evitals_30_col = evitals_30
  )

  # Check structure
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(all(c("measure", "pop", "numerator", "denominator", "prop", "prop_label") %in% names(result)))

  # Check calculations
  testthat::expect_equal(sum(result$numerator), 5)
  testthat::expect_equal(sum(result$denominator), 5)
  testthat::expect_equal(result$prop[result$pop == "All"], 1)
  testthat::expect_equal(nrow(result), 1)

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
    esituation_11 = c(rep("I60", 3), rep("I61", 2)),
    esituation_12 = c(rep("I63", 2), rep("I64", 3)),
  )

  vitals_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    evitals_23 = c(16, 15, 14, 13, 12),
    evitals_26 = c("Alert", "Painful", "Verbal", "Unresponsive", "Alert"),
    evitals_29 = rep("positive", 5),
    evitals_30 = rep("a pain scale", 5)
  )

  # test the success of the function

  result_2 <- stroke_01(patient_scene_table = patient_table,
                              response_table = response_table,
                              situation_table = situation_table,
                              vitals_table = vitals_table,
                              erecord_01_col = erecord_01,
                              eresponse_05_col = eresponse_05,
                              esituation_11_col = esituation_11,
                              esituation_12_col = esituation_12,
                              evitals_29_col = evitals_29,
                              evitals_23_col = evitals_23,
                              evitals_26_col = evitals_26,
                              evitals_30_col = evitals_30
                              )

  # Check calculations
  testthat::expect_equal(sum(result_2$numerator), 5)
  testthat::expect_equal(sum(result_2$denominator), 5)
  testthat::expect_equal(result_2$prop[result_2$pop == "All"], 1)
  testthat::expect_equal(nrow(result_2), 1)


})

testthat::test_that("stroke_01 handles missing data correctly", {

  # Synthetic test data
  missing_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    esituation_11 = c(rep("I60", 3), rep("I61", 2)),
    esituation_12 = c(rep("I63", 2), rep("I64", 3)),
    evitals_23 = c(16, 15, NA_integer_, 13, NA_integer_),
    evitals_26 = c("Alert", "Painful", "Verbal", NA_character_, NA_character_),
    evitals_29 = rep("positive", 5),
    evitals_30 = rep("a pain scale", 5)
  )

  result <- stroke_01(
    df = missing_data,
    erecord_01_col = erecord_01,
    eresponse_05_col = eresponse_05,
    esituation_11_col = esituation_11,
    esituation_12_col = esituation_12,
    evitals_29_col = evitals_29,
    evitals_23_col = evitals_23,
    evitals_26_col = evitals_26,
    evitals_30_col = evitals_30
  )

  testthat::expect_true(nrow(result) > 0)
  testthat::expect_true(all(!is.na(result$denominator)))

})

testthat::test_that("stroke_01 returns empty result for non-matching criteria", {

  # Synthetic test data
  non_matching_data <- tibble::tibble(
    erecord_01 = c("R1", "R2"),
    epatient_15 = c(30, 50),
    epatient_16 = c("Years", "Years"),
    eresponse_05 = c("Non-911 Call", "Non-911 Call"),
    esituation_11 = c("Non-Respiratory", "Non-Respiratory"),
    esituation_12 = c("Not Asthma", "Not Asthma"),
    evitals_29 = c(60, 59),
    evitals_23 = c(16, 15),
    evitals_26 = c("Alert", "Painful"),
    evitals_30 = rep("710925007", 2)
  )

  result <- stroke_01(
    df = non_matching_data,
    erecord_01_col = erecord_01,
    eresponse_05_col = eresponse_05,
    esituation_11_col = esituation_11,
    esituation_12_col = esituation_12,
    evitals_23_col = evitals_23,
    evitals_26_col = evitals_26,
    evitals_29_col = evitals_29,
    evitals_30_col = evitals_30
  )

  testthat::expect_equal(sum(result$denominator), 0)

})

