testthat::test_that("pediatrics_03b produces expected results", {

  # Synthetic test data
  test_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01", "2025-06-01", "2025-12-15")),
    patient_dob = as.Date(c("2021-01-01", "2020-01-01", "2022-02-01", "2023-06-01", "2019-12-15")),
    epatient_15 = c(4, 5, 3, 2, 6),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Years", "Years"),
    eresponse_05 = rep(2205001, 5),
    emedications_03 = rep("stuff", 5),
    emedications_04 = c("Inhalation", "pill", "liquid", "pill", "liquid"),
    eexam_01 = c(60, 59, 58, 57, 56),
    eexam_02 = c("Red", "Purple", "Grey", "Yellow", "Orange")
  )

  # Run function
  result <- pediatrics_03b(
    df = test_data,
    erecord_01_col = erecord_01,
    incident_date_col = incident_date,
    patient_DOB_col = patient_dob,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    emedications_03_col = emedications_03,
    emedications_04_col = emedications_04,
    eexam_01_col = eexam_01,
    eexam_02_col = eexam_02
  )

  # Check structure
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(all(c("measure", "pop", "numerator", "denominator", "prop", "prop_label") %in% names(result)))

  # Check calculations
  testthat::expect_equal(sum(result$numerator), 4)  # 5 total, 4 had weight-based meds
  testthat::expect_equal(sum(result$denominator), 4)  # Four cases met inclusion criteria
  testthat::expect_equal(result$prop[result$pop == "Peds"], 1)
  testthat::expect_equal(nrow(result), 1)

  # create tables to test correct functioning
  patient_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01", "2025-06-01", "2025-12-15")),
    patient_dob = as.Date(c("2021-01-01", "2020-01-01", "2022-02-01", "2023-06-01", "2019-12-15")),
    epatient_15 = c(4, 5, 3, 2, 6),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Years", "Years"),

  )

  response_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    eresponse_05 = rep(2205001, 5)

  )

  exam_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    eexam_01 = c(60, 59, 58, 57, 56),
    eexam_02 = c("Red", "Purple", "Grey", "Yellow", "Orange")
  )

  medications_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    emedications_03 = rep("stuff", 5),
    emedications_04 = c("Inhalation", "pill", "liquid", "pill", "liquid"),

  )

  # test the success of the function

  result_2 <- pediatrics_03b(patient_scene_table = patient_table,
                              response_table = response_table,
                              exam_table = exam_table,
                              medications_table = medications_table,
                             erecord_01_col = erecord_01,
                             incident_date_col = incident_date,
                             patient_DOB_col = patient_dob,
                             epatient_15_col = epatient_15,
                             epatient_16_col = epatient_16,
                             eresponse_05_col = eresponse_05,
                             emedications_03_col = emedications_03,
                             emedications_04_col = emedications_04,
                             eexam_01_col = eexam_01,
                             eexam_02_col = eexam_02
  )

  # Check calculations
  testthat::expect_equal(sum(result_2$numerator), 4)
  testthat::expect_equal(sum(result_2$denominator), 4)
  testthat::expect_equal(result_2$prop[result_2$pop == "Peds"], 1)
  testthat::expect_equal(nrow(result_2), 1)


})

testthat::test_that("pediatrics_03b handles missing data correctly", {

  # Synthetic test data
  missing_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01", "2025-06-01", "2025-12-15")),
    patient_dob = as.Date(c("2021-01-01", "2020-01-01", "2022-02-01", "2023-06-01", "2019-12-15")),
    epatient_15 = c(4, 5, 3, 2, 6),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Years", "Years"),
    eresponse_05 = rep(2205001, 5),
    emedications_03 = rep("stuff", 5),
    emedications_04 = c("Inhalation", "pill", "liquid", "pill", NA),
    eexam_01 = c(60, 59, 58, 57, 56),
    eexam_02 = c("Red", "Purple", "Grey", "Yellow", NA)
  )

  result <- pediatrics_03b(
    df = missing_data,
    erecord_01_col = erecord_01,
    incident_date_col = incident_date,
    patient_DOB_col = patient_dob,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    emedications_03_col = emedications_03,
    emedications_04_col = emedications_04,
    eexam_01_col = eexam_01,
    eexam_02_col = eexam_02
  )

  testthat::expect_true(nrow(result) > 0)
  testthat::expect_true(all(!is.na(result$denominator)))
  testthat::expect_equal(result$numerator, 4)
  testthat::expect_equal(result$denominator, 4)
  testthat::expect_equal(result$prop, 1)

})

testthat::test_that("pediatrics_03b returns empty result for non-matching criteria", {

  # Synthetic test data
  non_matching_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01", "2025-06-01", "2025-12-15")),
    patient_dob = as.Date(c("2021-01-01", "2020-01-01", "2022-02-01", "2023-06-01", "2019-12-15")),
    epatient_15 = c(4, 5, 3, 2, 6),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Years", "Years"),
    eresponse_05 = rep(2205001, 5),
    emedications_03 = rep("stuff", 5),
    emedications_04 = c("Inhalation", "Topical", "Inhalation", "Topical", "Topical"),
    eexam_01 = c(60, 59, 58, 57, 56),
    eexam_02 = c("Red", "Purple", "Grey", "Yellow", "Orange")
  )

  result <- pediatrics_03b(
    df = non_matching_data,
    erecord_01_col = erecord_01,
    incident_date_col = incident_date,
    patient_DOB_col = patient_dob,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    emedications_03_col = emedications_03,
    emedications_04_col = emedications_04,
    eexam_01_col = eexam_01,
    eexam_02_col = eexam_02
  )

  testthat::expect_equal(sum(result$denominator), 0)
})

