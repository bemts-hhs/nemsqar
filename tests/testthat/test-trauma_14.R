testthat::test_that("trauma_14 produces expected results", {

  # Synthetic test data
  test_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    eresponse_10 = rep(2210011, 5),
    esituation_02 = rep("Yes", 5),
    evitals_06 = c(100, 90, 80, 70, 85),
    evitals_10 = c(110, 89, 88, 71, 85),
    evitals_12 = c(50, 60, 70, 80, 75),
    evitals_14 = c(30, 9, 8, 7, 31),
    evitals_15 = c("apneic", "labored", "rapid", "shallow", "weak/agonal"),
    evitals_21 = c(5, 4, 3, 2, 1),
    eexam_16 = c(3516043, 3516067, 3516043, 3516067, 3516067),
    eexam_20 = c(3520045, 3520043, 3520019, 3520017, 3520017),
    eexam_23 = c(3523011, 3523003, 3523001, 3523011, 3523003),
    eexam_25 = c(3525039, 3525023, 3525005, 3525039, 3525023),
    edisposition_24 = c(4224017, 4224003, 4224017, 4224003, 4224017),
    edisposition_30 = c(4230001, 4230003, 4230001, 4230007, 4230007),
    eprocedures_03 = c(424979004, 427753009, 429705000, 47545007, 243142003),
    einjury_01 = c("V20", "V36", "V86", "V39", "V32"),
    einjury_03 = c(2903011, 2903009, 2903005, 3903003, 2903001),
    einjury_04 = c(2904013, 2904011, 2904009, 2904007, 2904001),
    einjury_09 = c(11, 12, 13, 14, 15)
  )

  # Run function with the first and last pain score columns
  result <- suppressWarnings(trauma_14(
    df = test_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    eresponse_10_col = eresponse_10,
    esituation_02_col = esituation_02,
    evitals_06_col = evitals_06,
    evitals_10_col = evitals_10,
    evitals_12_col = evitals_12,
    evitals_14_col = evitals_14,
    evitals_15_col = evitals_15,
    evitals_21_col = evitals_21,
    eexam_16_col = eexam_16,
    eexam_20_col = eexam_20,
    eexam_23_col = eexam_23,
    eexam_25_col = eexam_25,
    edisposition_24_col = edisposition_24,
    transport_disposition_col = edisposition_30,
    eprocedures_03_col = eprocedures_03,
    einjury_01_col = einjury_01,
    einjury_03_col = einjury_03,
    einjury_04_col = einjury_04,
    einjury_09_col = einjury_09,
    confidence_interval = TRUE
  ))

  # Check structure
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(all(c("measure", "pop", "numerator", "denominator", "prop", "prop_label", "lower_ci", "upper_ci") %in% names(result)))

  # should throw a warning due to small counts
  testthat::expect_warning(trauma_14(
    df = test_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    eresponse_10_col = eresponse_10,
    esituation_02_col = esituation_02,
    evitals_06_col = evitals_06,
    evitals_10_col = evitals_10,
    evitals_12_col = evitals_12,
    evitals_14_col = evitals_14,
    evitals_15_col = evitals_15,
    evitals_21_col = evitals_21,
    eexam_16_col = eexam_16,
    eexam_20_col = eexam_20,
    eexam_23_col = eexam_23,
    eexam_25_col = eexam_25,
    edisposition_24_col = edisposition_24,
    transport_disposition_col = edisposition_30,
    eprocedures_03_col = eprocedures_03,
    einjury_01_col = einjury_01,
    einjury_03_col = einjury_03,
    einjury_04_col = einjury_04,
    einjury_09_col = einjury_09,
    confidence_interval = TRUE
  ))

  # Run function with the first and last pain score columns
  result <- trauma_14(
    df = test_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    eresponse_10_col = eresponse_10,
    esituation_02_col = esituation_02,
    evitals_06_col = evitals_06,
    evitals_10_col = evitals_10,
    evitals_12_col = evitals_12,
    evitals_14_col = evitals_14,
    evitals_15_col = evitals_15,
    evitals_21_col = evitals_21,
    eexam_16_col = eexam_16,
    eexam_20_col = eexam_20,
    eexam_23_col = eexam_23,
    eexam_25_col = eexam_25,
    edisposition_24_col = edisposition_24,
    transport_disposition_col = edisposition_30,
    eprocedures_03_col = eprocedures_03,
    einjury_01_col = einjury_01,
    einjury_03_col = einjury_03,
    einjury_04_col = einjury_04,
    einjury_09_col = einjury_09
  )

  # Check structure
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(all(c("measure", "pop", "numerator", "denominator", "prop", "prop_label") %in% names(result)))

  # Check calculations
  testthat::expect_equal(sum(result$numerator), 5)
  testthat::expect_equal(sum(result$denominator), 5)
  testthat::expect_equal(result$prop[result$pop == "10-64 yrs"], 1)
  testthat::expect_equal(nrow(result), 3)

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
    eresponse_10 = rep(2210011, 5)
  )

  # situation table
  situation_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    esituation_02 = rep("Yes", 5),
  )

  # vitals table
  vitals_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    evitals_06 = c(100, 90, 80, 70, 85),
    evitals_10 = c(110, 89, 88, 71, 85),
    evitals_12 = c(50, 60, 70, 80, 75),
    evitals_14 = c(30, 9, 8, 7, 31),
    evitals_15 = c("apneic", "labored", "rapid", "shallow", "weak/agonal"),
    evitals_21 = c(5, 4, 3, 2, 1)
  )

  # disposition table
  disposition_table <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    edisposition_24 = c(4224017, 4224003, 4224017, 4224003, 4224017),
    edisposition_30 = c(4230001, 4230003, 4230001, 4230007, 4230007)
  )

  # injury table
  injury_table <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    einjury_01 = c("V20", "V36", "V86", "V39", "V32"),
    einjury_03 = c(2903011, 2903009, 2903005, 3903003, 2903001),
    einjury_04 = c(2904013, 2904011, 2904009, 2904007, 2904001),
    einjury_09 = c(11, 12, 13, 14, 15)
  )

  # exam table
  exam_table <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    eexam_16 = c(3516043, 3516067, 3516043, 3516067, 3516067),
    eexam_20 = c(3520045, 3520043, 3520019, 3520017, 3520017),
    eexam_23 = c(3523011, 3523003, 3523001, 3523011, 3523003),
    eexam_25 = c(3525039, 3525023, 3525005, 3525039, 3525023)
  )

  # procedures table
  procedures_table <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    eprocedures_03 = c(424979004, 427753009, 429705000, 47545007, 243142003)
  )

  # test the success of the function
  result <- trauma_14(patient_scene_table = patient_table,
                      response_table = response_table,
                      situation_table = situation_table,
                      vitals_table = vitals_table,
                      disposition_table = disposition_table,
                      exam_table = exam_table,
                      injury_table = injury_table,
                      procedures_table = procedures_table,
                      erecord_01_col = erecord_01,
                      incident_date_col = incident_date,
                      patient_DOB_col = patient_dob,
                      epatient_15_col = epatient_15,
                      epatient_16_col = epatient_16,
                      eresponse_05_col = eresponse_05,
                      eresponse_10_col = eresponse_10,
                      esituation_02_col = esituation_02,
                      evitals_06_col = evitals_06,
                      evitals_10_col = evitals_10,
                      evitals_12_col = evitals_12,
                      evitals_14_col = evitals_14,
                      evitals_15_col = evitals_15,
                      evitals_21_col = evitals_21,
                      eexam_16_col = eexam_16,
                      eexam_20_col = eexam_20,
                      eexam_23_col = eexam_23,
                      eexam_25_col = eexam_25,
                      edisposition_24_col = edisposition_24,
                      transport_disposition_col = edisposition_30,
                      eprocedures_03_col = eprocedures_03,
                      einjury_01_col = einjury_01,
                      einjury_03_col = einjury_03,
                      einjury_04_col = einjury_04,
                      einjury_09_col = einjury_09
  )

  # Check calculations
  testthat::expect_equal(sum(result$numerator), 5)
  testthat::expect_equal(sum(result$denominator),5)
  testthat::expect_equal(result$prop[result$pop == "10-64 yrs"], 1)
  testthat::expect_equal(nrow(result), 3)

})

testthat::test_that("trauma_14 handles missing data correctly", {

  # Synthetic test data
  test_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    eresponse_10 = rep(2210011, 5),
    esituation_02 = c("Yes", "Yes", "Yes", NA, NA),
    evitals_06 = c(100, 90, 80, 70, 85),
    evitals_10 = c(110, 89, 88, 71, 85),
    evitals_12 = c(50, 60, 70, 80, 75),
    evitals_14 = c(30, 9, 8, 7, 31),
    evitals_15 = c("apneic", "labored", "rapid", "shallow", "weak/agonal"),
    evitals_21 = c(5, 4, 3, 2, 1),
    eexam_16 = c(3516043, 3516067, 3516043, 3516067, 3516067),
    eexam_20 = c(3520045, 3520043, 3520019, 3520017, 3520017),
    eexam_23 = c(3523011, 3523003, 3523001, 3523011, 3523003),
    eexam_25 = c(3525039, 3525023, 3525005, 3525039, 3525023),
    edisposition_24 = c(4224017, 4224003, 4224017, 4224003, 4224017),
    edisposition_30 = c(NA, NA, 4230001, 4230007, 4230007),
    eprocedures_03 = c(424979004, 427753009, 429705000, 47545007, 243142003),
    einjury_01 = c("V20", "V36", "V86", "V39", "V32"),
    einjury_03 = c(2903011, 2903009, 2903005, 3903003, 2903001),
    einjury_04 = c(2904013, 2904011, 2904009, 2904007, 2904001),
    einjury_09 = c(11, 12, 13, 14, 15)
  )

  # run the function with first and last
  # pain scale columns
  result <- trauma_14(
    df = test_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    eresponse_10_col = eresponse_10,
    esituation_02_col = esituation_02,
    evitals_06_col = evitals_06,
    evitals_10_col = evitals_10,
    evitals_12_col = evitals_12,
    evitals_14_col = evitals_14,
    evitals_15_col = evitals_15,
    evitals_21_col = evitals_21,
    eexam_16_col = eexam_16,
    eexam_20_col = eexam_20,
    eexam_23_col = eexam_23,
    eexam_25_col = eexam_25,
    edisposition_24_col = edisposition_24,
    transport_disposition_col = edisposition_30,
    eprocedures_03_col = eprocedures_03,
    einjury_01_col = einjury_01,
    einjury_03_col = einjury_03,
    einjury_04_col = einjury_04,
    einjury_09_col = einjury_09
  )

  testthat::expect_true(nrow(result) > 0)
  testthat::expect_true(all(!is.na(result$denominator)))

})

testthat::test_that("trauma_14 returns empty result for non-matching criteria", {

  # Synthetic test data
  test_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    eresponse_10 = rep(2210011, 5),
    esituation_02 = rep("No", 5),
    evitals_06 = c(100, 90, 80, 70, 85),
    evitals_10 = c(110, 89, 88, 71, 85),
    evitals_12 = c(50, 60, 70, 80, 75),
    evitals_14 = c(30, 9, 8, 7, 31),
    evitals_15 = c("apneic", "labored", "rapid", "shallow", "weak/agonal"),
    evitals_21 = c(5, 4, 3, 2, 1),
    eexam_16 = c(3516043, 3516067, 3516043, 3516067, 3516067),
    eexam_20 = c(3520045, 3520043, 3520019, 3520017, 3520017),
    eexam_23 = c(3523011, 3523003, 3523001, 3523011, 3523003),
    eexam_25 = c(3525039, 3525023, 3525005, 3525039, 3525023),
    edisposition_24 = c(4224017, 4224003, 4224017, 4224003, 4224017),
    edisposition_30 = c(4230001, 4230003, 4230001, 4230007, 4230007),
    eprocedures_03 = c(424979004, 427753009, 429705000, 47545007, 243142003),
    einjury_01 = c("V20", "V36", "V86", "V39", "V32"),
    einjury_03 = c(2903011, 2903009, 2903005, 3903003, 2903001),
    einjury_04 = c(2904013, 2904011, 2904009, 2904007, 2904001),
    einjury_09 = c(11, 12, 13, 14, 15)
  )

  # run the function with the
  # initial and last pain scale
  # columns
  result <- trauma_14(
    df = test_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    eresponse_10_col = eresponse_10,
    esituation_02_col = esituation_02,
    evitals_06_col = evitals_06,
    evitals_10_col = evitals_10,
    evitals_12_col = evitals_12,
    evitals_14_col = evitals_14,
    evitals_15_col = evitals_15,
    evitals_21_col = evitals_21,
    eexam_16_col = eexam_16,
    eexam_20_col = eexam_20,
    eexam_23_col = eexam_23,
    eexam_25_col = eexam_25,
    edisposition_24_col = edisposition_24,
    transport_disposition_col = edisposition_30,
    eprocedures_03_col = eprocedures_03,
    einjury_01_col = einjury_01,
    einjury_03_col = einjury_03,
    einjury_04_col = einjury_04,
    einjury_09_col = einjury_09
  )

  testthat::expect_equal(sum(result$denominator), 0)

})

