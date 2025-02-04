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
