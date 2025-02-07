testthat::test_that("asthma_01 produces expected results", {

  # Synthetic test data
  test_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = c("911 Call", "911 Call", "911 Call", "911 Call", "911 Call"),
    esituation_11 = c("Respiratory Distress", "Respiratory Distress", "Chest Pain", "Respiratory Distress", "Respiratory Distress"),
    esituation_12 = c("Asthma", "Asthma", "Not Asthma", "Asthma", "Asthma"),
    emedications_03 = c("Albuterol", "Albuterol", "Epinephrine", "None", "Albuterol")
  )

  # Run function
  result <- asthma_01(
    df = test_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    esituation_11_col = esituation_11,
    esituation_12_col = esituation_12,
    emedications_03_col = emedications_03
  )

  # Check structure
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(all(c("measure", "pop", "numerator", "denominator", "prop", "prop_label") %in% names(result)))

  # Check calculations
  testthat::expect_equal(sum(result$numerator), 0)  # Three cases had Albuterol
  testthat::expect_equal(sum(result$denominator), 0)  # Four cases met inclusion criteria
  testthat::expect_equal(result$prop[result$pop == "All"], 0)
  testthat::expect_equal(nrow(result), 3)

  # create tables to test correct functioning
  patient_table <- tibble::tibble(

    erecord_01 = 1:3,
    incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01")),
    patient_dob = as.Date(c("2000-01-01", "2020-01-01", "2023-01-01")),
    epatient_15 = c(25, 5, 2),
    epatient_16 = c("years", "years", "months")

  )

  response_table <- tibble::tibble(

    erecord_01 = 1:3,
    eresponse_05 = c("2205001", "2205009", "2205003")

  )

  situation_table <- tibble::tibble(

    erecord_01 = 1:3,
    esituation_11 = c("weakness", "asthma", "bronchospasm"),
    esituation_12 = c("asthma", "weakness", "weakness")
  )

  medications_table <- tibble::tibble(

    erecord_01 = 1:3,
    emedications_03 = c("albuterol", "levalbuterol", "metaproterenol")

  )

  # test the success of the function

  result_2 <- asthma_01(patient_scene_table = patient_table,
                                 response_table = response_table,
                                 situation_table = situation_table,
                                 medications_table = medications_table,
                                 erecord_01_col = erecord_01,
                                 incident_date_col = incident_date,
                                 patient_dob_col = patient_dob,
                                 epatient_15_col = epatient_15,
                                 epatient_16_col = epatient_16,
                                 eresponse_05_col = eresponse_05,
                                 esituation_11_col = esituation_11,
                                 esituation_12_col = esituation_12,
                                 emedications_03_col = emedications_03
                      )

  # Check calculations
  testthat::expect_equal(sum(result_2$numerator), 4)  # Three cases had Albuterol
  testthat::expect_equal(sum(result_2$denominator), 4)  # Four cases met inclusion criteria
  testthat::expect_equal(result_2$prop[result_2$pop == "All"], 1)
  testthat::expect_equal(nrow(result_2), 3)


})

testthat::test_that("asthma_01 handles missing data correctly", {
  missing_data <- tibble::tibble(
    erecord_01 = c("R1", "R2"),
    epatient_15 = c(NA, 30),
    epatient_16 = c("Years", NA),
    eresponse_05 = c("911 Call", "911 Call"),
    esituation_11 = c("Respiratory Distress", "Respiratory Distress"),
    esituation_12 = c("Asthma", "Asthma"),
    emedications_03 = c(NA, "Albuterol")
  )

  result <- asthma_01(
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

testthat::test_that("asthma_01 returns empty result for non-matching criteria", {
  non_matching_data <- tibble::tibble(
    erecord_01 = c("R1", "R2"),
    epatient_15 = c(30, 50),
    epatient_16 = c("Years", "Years"),
    eresponse_05 = c("Non-911 Call", "Non-911 Call"),
    esituation_11 = c("Non-Respiratory", "Non-Respiratory"),
    esituation_12 = c("Not Asthma", "Not Asthma"),
    emedications_03 = c("None", "None")
  )

  result <- asthma_01(
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
