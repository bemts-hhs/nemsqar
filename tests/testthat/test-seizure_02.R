testthat::test_that("seizure_02 produces expected results", {

  # Synthetic test data
  test_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    esituation_11 = rep("G40", 5),
    esituation_12 = rep("r56", 5),
    emedications_03 = rep(3322, 5)
  )

  # Run function
  result <- seizure_02(
    df = test_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    esituation_11_col = esituation_11,
    esituation_12_col = esituation_12,
    emedications_03_col = emedications_03,
  )

  # Check structure
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(all(c("measure", "pop", "numerator", "denominator", "prop", "prop_label") %in% names(result)))

  # Check calculations
  testthat::expect_equal(sum(result$numerator), 9)  # Three cases had Albuterol
  testthat::expect_equal(sum(result$denominator), 9)  # Four cases met inclusion criteria
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

  situation_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    esituation_11 = rep("G40", 5),
    esituation_12 = rep("r56", 5),
  )

  medications_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    emedications_03 = rep(3322, 5)

  )

  # test the success of the function

  result_2 <- seizure_02(patient_scene_table = patient_table,
                              response_table = response_table,
                              situation_table = situation_table,
                              medications_table = medications_table,
                              erecord_01_col = erecord_01,
                              incident_date_col = incident_date,
                              patient_DOB_col = patient_dob,
                              epatient_15_col = epatient_15,
                              epatient_16_col = epatient_16,
                              eresponse_05_col = eresponse_05,
                              esituation_11_col = esituation_11,
                              esituation_12_col = esituation_12,
                              emedications_03_col = emedications_03
                         )

  # Check calculations
  testthat::expect_equal(sum(result_2$numerator), 10)  # Three cases had Albuterol
  testthat::expect_equal(sum(result_2$denominator), 10)  # Four cases met inclusion criteria
  testthat::expect_equal(result_2$prop[result_2$pop == "All"], 1)
  testthat::expect_equal(nrow(result_2), 3)


})

testthat::test_that("seizure_02 handles missing data correctly", {

  # Synthetic test data
  missing_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    esituation_11 = rep("G40", 5),
    esituation_12 = rep(NA_character_, 5),
    emedications_03 = c(rep(3322, 3), rep(NA_integer_, 2))
  )

  result <- seizure_02(
    df = missing_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    esituation_11_col = esituation_11,
    esituation_12_col = esituation_12,
    emedications_03_col = emedications_03
  )

  testthat::expect_true(nrow(result) > 0)
  testthat::expect_true(all(!is.na(result$denominator)))

})

testthat::test_that("seizure_02 returns empty result for non-matching criteria", {

  # Synthetic test data
  non_matching_data <- tibble::tibble(
    erecord_01 = c("R1", "R2"),
    epatient_15 = c(30, 50),
    epatient_16 = c("Years", "Years"),
    eresponse_05 = c("Non-911 Call", "Non-911 Call"),
    esituation_11 = c("Non-Respiratory", "Non-Respiratory"),
    esituation_12 = c("Not Asthma", "Not Asthma"),
    emedications_03 = c("None", "None")
  )

  result <- seizure_02(
    df = non_matching_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    esituation_11_col = esituation_11,
    esituation_12_col = esituation_12,
    emedications_03_col = emedications_03
  )

  testthat::expect_equal(sum(result$denominator), 0)
})

