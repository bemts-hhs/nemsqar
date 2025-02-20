testthat::test_that("trauma_14_population rejects invalid argument combinations", {

  testthat::expect_error(trauma_14_population(df = tibble::tibble(),
                                              patient_scene_table = tibble::tibble(),
                                              response_table = tibble::tibble(),
                                              situation_table = tibble::tibble(),
                                              vitals_table = tibble::tibble(),
                                              disposition_table = tibble::tibble(),
                                              procedures_table = tibble::tibble(),
                                              injury_table = tibble::tibble(),
                                              exam_table = tibble::tibble(),
                                              erecord_01_col = character(),
                                              epatient_15_col = numeric(),
                                              epatient_16_col = character(),
                                              eresponse_05_col = character(),
                                              eresponse_10_col = character(),
                                              esituation_02_col = character(),
                                              evitals_06_col = numeric(),
                                              evitals_10_col = numeric(),
                                              evitals_12_col = numeric(),
                                              evitals_14_col = numeric(),
                                              evitals_15_col = character(),
                                              evitals_21_col = character(),
                                              eexam_16_col = character(),
                                              eexam_20_col = character(),
                                              eexam_23_col = character(),
                                              eexam_25_col = character(),
                                              edisposition_24_col = character(),
                                              transport_disposition_col = character(),
                                              eprocedures_03_col = character(),
                                              einjury_01_col = character(),
                                              einjury_03_col = character(),
                                              einjury_04_col = character(),
                                              einjury_09_col = numeric()
  ),
  "will only work by passing a"
  )

  testthat::expect_error(trauma_14_population(patient_scene_table = list(),
                                              response_table = tibble::tibble(),
                                              situation_table = tibble::tibble(),
                                              vitals_table = tibble::tibble(),
                                              disposition_table = tibble::tibble(),
                                              procedures_table = tibble::tibble(),
                                              injury_table = tibble::tibble(),
                                              exam_table = tibble::tibble(),
                                              erecord_01_col = character(),
                                              incident_date_col = date(),
                                              patient_DOB_col = date(),
                                              epatient_15_col = numeric(),
                                              epatient_16_col = character(),
                                              eresponse_05_col = character(),
                                              eresponse_10_col = character(),
                                              esituation_02_col = character(),
                                              evitals_06_col = numeric(),
                                              evitals_10_col = numeric(),
                                              evitals_12_col = numeric(),
                                              evitals_14_col = numeric(),
                                              evitals_15_col = character(),
                                              evitals_21_col = character(),
                                              eexam_16_col = character(),
                                              eexam_20_col = character(),
                                              eexam_23_col = character(),
                                              eexam_25_col = character(),
                                              edisposition_24_col = character(),
                                              transport_disposition_col = character(),
                                              eprocedures_03_col = character(),
                                              einjury_01_col = character(),
                                              einjury_03_col = character(),
                                              einjury_04_col = character(),
                                              einjury_09_col = numeric()
  ),
  "An object of class"
  )

})

testthat::test_that("trauma_14_population rejects missing required column arguments", {
  testthat::expect_error(trauma_14_population(df = tibble::tibble(), epatient_15_col = "Age"),
                         "One or more of the \\*_col arguments is missing")
})

testthat::test_that("trauma_14_population rejects non-dataframe inputs", {
  testthat::expect_error(trauma_14_population(df = list()),
                         "One or more")

  testthat::expect_error(trauma_14_population(patient_scene_table = matrix()),
                         "One or more")
})

testthat::test_that("trauma_14_population validates date column formats", {

  df <- tibble::tibble(erecord_01 = character(),
                       incident_date = character(),
                       patient_dob = character(),
                       epatient_15 = numeric(),
                       epatient_16 = character(),
                       eresponse_05 = character(),
                       eresponse_10 = character(),
                       esituation_02 = character(),
                       evitals_06 = numeric(),
                       evitals_10 = numeric(),
                       evitals_12 = numeric(),
                       evitals_14 = numeric(),
                       evitals_15 = character(),
                       evitals_21 = character(),
                       eexam_16 = character(),
                       eexam_20 = character(),
                       eexam_23 = character(),
                       eexam_25 = character(),
                       edisposition_24 = character(),
                       transport_disposition = character(),
                       eprocedures_03 = character(),
                       einjury_01 = character(),
                       einjury_03 = character(),
                       einjury_04 = character(),
                       einjury_09 = numeric()
  )

  testthat::expect_error(
    trauma_14_population(
      df,
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
  )

  testthat::expect_error(
    trauma_14_population(
      patient_scene_table = tibble::tibble(),
      response_table = tibble::tibble(),
      situation_table = tibble::tibble(),
      vitals_table = tibble::tibble(),
      disposition_table = tibble::tibble(),
      procedures_table = tibble::tibble(),
      injury_table = tibble::tibble(),
      exam_table = tibble::tibble(),
      erecord_01_col = character(),
      incident_date_col = character(),
      patient_DOB_col = character(),
      epatient_15_col = numeric(),
      epatient_16_col = character(),
      eresponse_05_col = character(),
      eresponse_10_col = character(),
      esituation_02_col = character(),
      evitals_06_col = numeric(),
      evitals_10_col = numeric(),
      evitals_12_col = numeric(),
      evitals_14_col = numeric(),
      evitals_15_col = character(),
      evitals_21_col = character(),
      eexam_16_col = character(),
      eexam_20_col = character(),
      eexam_23_col = character(),
      eexam_25_col = character(),
      edisposition_24_col = character(),
      transport_disposition_col = character(),
      eprocedures_03_col = character(),
      einjury_01_col = character(),
      einjury_03_col = character(),
      einjury_04_col = character(),
      einjury_09_col = numeric()
    )
  )

})

testthat::test_that("trauma_14_population fails with unknown columns", {

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

  testthat::expect_error(trauma_14_population(test_data,
                                              erecord_01_col = erecord_01,
                                              incident_date_col = NULL,
                                              patient_DOB_col = NULL,
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
                                              einjury_04_col = "dummy",
                                              einjury_09_col = "dummy"
  ),
  "exist"
  )

})


testthat::test_that("trauma_14_population correctly classifies patient age", {

  # Synthetic test data
  test_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01", "2025-01-01", "2025-06-01")),
    patient_dob = as.Date(c("2000-01-01", "2020-01-01", "2023-02-01", "2023-01-01", "1970-06-01")),
    epatient_15 = c(25, 5, 2, 2, 55),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Years", "Years"),
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

  result <- trauma_14_population(test_data,
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

  testthat::expect_true(all(result$adults$system_age_adult == TRUE))
  testthat::expect_true(all(result$adults$system_age_minor == FALSE))

  result <- trauma_14_population(test_data,
                                 erecord_01_col = erecord_01,
                                 incident_date_col = NULL,
                                 patient_DOB_col = NULL,
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

  testthat::expect_true(all(result$adults$system_age_adult == TRUE))
  testthat::expect_true(all(result$adults$system_age_minor == FALSE))

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

  result <- trauma_14_population(patient_scene_table = patient_table,
                                 response_table = response_table,
                                 situation_table = situation_table,
                                 vitals_table = vitals_table,
                                 disposition_table = disposition_table,
                                 procedures_table = procedures_table,
                                 exam_table = exam_table,
                                 injury_table = injury_table,
                                 erecord_01_col = erecord_01,
                                 incident_date_col = NULL,
                                 patient_DOB_col = NULL,
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

  testthat::expect_true(all(result$adults$system_age_adult == TRUE))
  testthat::expect_true(all(result$adults$system_age_minor == FALSE))

  result <- trauma_14_population(patient_scene_table = patient_table,
                                 response_table = response_table,
                                 situation_table = situation_table,
                                 vitals_table = vitals_table,
                                 disposition_table = disposition_table,
                                 procedures_table = procedures_table,
                                 exam_table = exam_table,
                                 injury_table = injury_table,
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

  testthat::expect_true(all(result$adults$system_age_adult == TRUE))
  testthat::expect_true(all(result$adults$system_age_minor == FALSE))

})

testthat::test_that("trauma_14_population correctly filters 911 calls", {

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

  result <- trauma_14_population(test_data,
                                 erecord_01_col = erecord_01,
                                 incident_date_col = NULL,
                                 patient_DOB_col = NULL,
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

  emergency_calls <- result$filter_process |>
    dplyr::filter(filter == "911 calls") |>
    dplyr::pull(count)

  testthat::expect_equal(nrow(result$filter_process), 30)
  testthat::expect_equal(emergency_calls, 5)

})

testthat::test_that("trauma_14_population runs correctly with table inputs", {

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

  result <- trauma_14_population(patient_scene_table = patient_table,
                                 response_table = response_table,
                                 situation_table = situation_table,
                                 vitals_table = vitals_table,
                                 disposition_table = disposition_table,
                                 procedures_table = procedures_table,
                                 exam_table = exam_table,
                                 injury_table = injury_table,
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

  testthat::expect_equal(nrow(result$filter_process), 30)
  testthat::expect_true(is.list(result))

  result <- trauma_14_population(patient_scene_table = patient_table,
                                 response_table = response_table,
                                 situation_table = situation_table,
                                 vitals_table = vitals_table,
                                 disposition_table = disposition_table,
                                 procedures_table = procedures_table,
                                 exam_table = exam_table,
                                 injury_table = injury_table,
                                 erecord_01_col = erecord_01,
                                 incident_date_col = NULL,
                                 patient_DOB_col = NULL,
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

  testthat::expect_equal(nrow(result$filter_process), 30)
  testthat::expect_true(is.list(result))

})
