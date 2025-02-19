testthat::test_that("trauma_08_population rejects invalid argument combinations", {

  testthat::expect_error(trauma_08_population(df = tibble::tibble(),
                                              patient_scene_table = tibble::tibble(),
                                              response_table = tibble::tibble(),
                                              situation_table = tibble::tibble(),
                                              vitals_table = tibble::tibble(),
                                              disposition_table = tibble::tibble(),
                                              erecord_01_col = character(),
                                              epatient_15_col = numeric(),
                                              epatient_16_col = character(),
                                              eresponse_05_col = character(),
                                              esituation_02_col = character(),
                                              evitals_06_col = numeric(),
                                              evitals_14_col = numeric(),
                                              evitals_23_col = character(),
                                              transport_disposition_col = character()
                                              ),
                         "Please choose one approach."
                         )

  testthat::expect_error(trauma_08_population(patient_scene_table = list(),
                                              response_table = tibble::tibble(),
                                              situation_table = tibble::tibble(),
                                              vitals_table = tibble::tibble(),
                                              disposition_table = tibble::tibble(),
                                              erecord_01_col = character(),
                                              epatient_15_col = numeric(),
                                              epatient_16_col = character(),
                                              eresponse_05_col = character(),
                                              esituation_02_col = character(),
                                              evitals_06_col = numeric(),
                                              evitals_14_col = numeric(),
                                              evitals_23_col = character(),
                                              transport_disposition_col = character()
                                              ),
                         "One or more of the tables"
                         )

})

testthat::test_that("trauma_08_population rejects missing required column arguments", {
  testthat::expect_error(trauma_08_population(df = tibble::tibble()),
                         "One or more of the \\*_col arguments is missing")
})

testthat::test_that("trauma_08_population rejects non-dataframe inputs", {
  testthat::expect_error(trauma_08_population(df = list()),
                         "One or more")

  testthat::expect_error(trauma_08_population(patient_scene_table = matrix()),
                         "One or more")
})

testthat::test_that("trauma_08_population validates date column formats", {

  df <- tibble::tibble(erecord_01_col = character(),
                       epatient_15_col = numeric(),
                       epatient_16_col = character(),
                       eresponse_05_col = character(),
                       esituation_02_col = character(),
                       evitals_06_col = numeric(),
                       evitals_14_col = numeric(),
                       evitals_23_col = character(),
                       transport_disposition_col = character()
                       )

  testthat::expect_error(
    trauma_08_population(
      df,
      erecord_01_col = erecord_01,
      epatient_15_col = epatient_15,
      epatient_16_col = epatient_16,
      eresponse_05_col = eresponse_05,
      esituation_02_col = esituation_02,
      evitals_06_col = evitals_06,
      evitals_14_col = evitals_14,
      evitals_23_col = evitals_23,
      transport_disposition_col = edisposition_30
    )
  )

  testthat::expect_error(
    trauma_08_population(
      patient_scene_table = tibble::tibble(),
      response_table = tibble::tibble(),
      situation_table = tibble::tibble(),
      vitals_table = tibble::tibble(),
      disposition_table = tibble::tibble(),
      erecord_01_col = character(),
      epatient_15_col = numeric(),
      epatient_16_col = character(),
      eresponse_05_col = character(),
      esituation_02_col = character(),
      evitals_06_col = numeric(),
      evitals_14_col = numeric(),
      evitals_23_col = character(),
      transport_disposition_col = character()
    )
  )

})

testthat::test_that("trauma_08_population fails with unknown columns", {

  # Synthetic test data
  test_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    esituation_02 = rep("Yes", 5),
    evitals_06 = c(100, 90, 80, 70, 85),
    evitals_14 = c(30, 9, 8, 7, 31),
    evitals_23 = c(6, 7, 8, 8, 7),
    edisposition_30 = c(4230001, 4230003, 4230001, 4230007, 4230007)
  )

  testthat::expect_error(trauma_08_population(test_data,
                                              erecord_01_col = erecord_01,
                                              incident_date_col = NULL,
                                              patient_DOB_col = NULL,
                                              epatient_15_col = epatient_15,
                                              epatient_16_col = epatient_16,
                                              eresponse_05_col = eresponse_05,
                                              esituation_02_col = esituation_02,
                                              evitals_06_col = evitals_06,
                                              evitals_14_col = evitals_14,
                                              evitals_23_col = evitals_23,
                                              transport_disposition_col = "dummy"
                                              ),
                         "exist"
                         )

})


testthat::test_that("trauma_08_population correctly classifies patient age", {

  # Synthetic test data
  test_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    esituation_02 = rep("Yes", 5),
    evitals_06 = c(100, 90, 80, 70, 85),
    evitals_14 = c(30, 9, 8, 7, 31),
    evitals_23 = c(6, 7, 8, 8, 7),
    edisposition_30 = c(4230001, 4230003, 4230001, 4230007, 4230007)
  )

  result <- trauma_08_population(test_data,
                                 erecord_01_col = erecord_01,
                                 epatient_15_col = epatient_15,
                                 epatient_16_col = epatient_16,
                                 eresponse_05_col = eresponse_05,
                                 esituation_02_col = esituation_02,
                                 evitals_06_col = evitals_06,
                                 evitals_14_col = evitals_14,
                                 evitals_23_col = evitals_23,
                                 transport_disposition_col = edisposition_30
                                 )

  testthat::expect_true(all(result$adults$system_age_adult == TRUE))
  testthat::expect_true(all(result$adults$system_age_minor == FALSE))

  result <- trauma_08_population(test_data,
                                 erecord_01_col = erecord_01,
                                 incident_date_col = NULL,
                                 patient_DOB_col = NULL,
                                 epatient_15_col = epatient_15,
                                 epatient_16_col = epatient_16,
                                 eresponse_05_col = eresponse_05,
                                 esituation_02_col = esituation_02,
                                 evitals_06_col = evitals_06,
                                 evitals_14_col = evitals_14,
                                 evitals_23_col = evitals_23,
                                 transport_disposition_col = edisposition_30

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
    eresponse_05 = rep(2205001, 5)
  )

  # situation table
  situation_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    esituation_02 = rep("Yes", 5)
  )

  # vitals table
  vitals_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    evitals_06 = c(100, 90, 80, 70, 85),
    evitals_14 = c(30, 9, 8, 7, 31),
    evitals_23 = c(6, 7, 8, 8, 7),
  )

  # disposition table
  disposition_table <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    edisposition_30 = c(4230001, 4230003, 4230001, 4230007, 4230007)
  )

  result <- trauma_08_population(patient_scene_table = patient_table,
                                 response_table = response_table,
                                 situation_table = situation_table,
                                 vitals_table = vitals_table,
                                 disposition_table = disposition_table,
                                 erecord_01_col = erecord_01,
                                 incident_date_col = NULL,
                                 patient_DOB_col = NULL,
                                 epatient_15_col = epatient_15,
                                 epatient_16_col = epatient_16,
                                 eresponse_05_col = eresponse_05,
                                 esituation_02_col = esituation_02,
                                 evitals_06_col = evitals_06,
                                 evitals_14_col = evitals_14,
                                 evitals_23_col = evitals_23,
                                 transport_disposition_col = edisposition_30
                                 )

  testthat::expect_true(all(result$adults$system_age_adult == TRUE))
  testthat::expect_true(all(result$adults$system_age_minor == FALSE))

  result <- trauma_08_population(patient_scene_table = patient_table,
                                 response_table = response_table,
                                 situation_table = situation_table,
                                 vitals_table = vitals_table,
                                 disposition_table = disposition_table,
                                 erecord_01_col = erecord_01,
                                 incident_date_col = incident_date,
                                 patient_DOB_col = patient_dob,
                                 epatient_15_col = epatient_15,
                                 epatient_16_col = epatient_16,
                                 eresponse_05_col = eresponse_05,
                                 esituation_02_col = esituation_02,
                                 evitals_06_col = evitals_06,
                                 evitals_14_col = evitals_14,
                                 evitals_23_col = evitals_23,
                                 transport_disposition_col = edisposition_30

  )

  testthat::expect_true(all(result$adults$system_age_adult == TRUE))
  testthat::expect_true(all(result$adults$system_age_minor == FALSE))

})

testthat::test_that("trauma_08_population correctly filters 911 calls", {

  # Synthetic test data
  test_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    esituation_02 = rep("Yes", 5),
    evitals_06 = c(100, 90, 80, 70, 85),
    evitals_14 = c(30, 9, 8, 7, 31),
    evitals_23 = c(6, 7, 8, 8, 7),
    edisposition_30 = c(4230001, 4230003, 4230001, 4230007, 4230007)
  )

  result <- trauma_08_population(test_data,
                                 erecord_01_col = erecord_01,
                                 incident_date_col = NULL,
                                 patient_DOB_col = NULL,
                                 epatient_15_col = epatient_15,
                                 epatient_16_col = epatient_16,
                                 eresponse_05_col = eresponse_05,
                                 esituation_02_col = esituation_02,
                                 evitals_06_col = evitals_06,
                                 evitals_14_col = evitals_14,
                                 evitals_23_col = evitals_23,
                                 transport_disposition_col = edisposition_30
                                 )

  emergency_calls <- result$filter_process |>
    dplyr::filter(filter == "911 calls") |>
    dplyr::pull(count)

  testthat::expect_equal(nrow(result$filter_process), 8)
  testthat::expect_equal(emergency_calls, 5)

})

testthat::test_that("trauma_08_population runs correctly with table inputs", {

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
    eresponse_05 = rep(2205001, 5)
  )

  # situation table
  situation_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    esituation_02 = rep("Yes", 5)
  )

  # vitals table
  vitals_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    evitals_06 = c(100, 90, 80, 70, 85),
    evitals_14 = c(30, 9, 8, 7, 31),
    evitals_23 = c(6, 7, 8, 8, 7),
  )

  # disposition table
  disposition_table <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    edisposition_30 = c(4230001, 4230003, 4230001, 4230007, 4230007)
  )

  # test the success of the function

  result <- trauma_08_population(patient_scene_table = patient_table,
                                 response_table = response_table,
                                 situation_table = situation_table,
                                 vitals_table = vitals_table,
                                 disposition_table = disposition_table,
                                 erecord_01_col = erecord_01,
                                 incident_date_col = incident_date,
                                 patient_DOB_col = patient_dob,
                                 epatient_15_col = epatient_15,
                                 epatient_16_col = epatient_16,
                                 eresponse_05_col = eresponse_05,
                                 esituation_02_col = esituation_02,
                                 evitals_06_col = evitals_06,
                                 evitals_14_col = evitals_14,
                                 evitals_23_col = evitals_23,
                                 transport_disposition_col = edisposition_30
                                 )

  testthat::expect_equal(nrow(result$filter_process), 8)
  testthat::expect_true(is.list(result))

  result <- trauma_08_population(patient_scene_table = patient_table,
                                 response_table = response_table,
                                 situation_table = situation_table,
                                 vitals_table = vitals_table,
                                 disposition_table = disposition_table,
                                 erecord_01_col = erecord_01,
                                 incident_date_col = NULL,
                                 patient_DOB_col = NULL,
                                 epatient_15_col = epatient_15,
                                 epatient_16_col = epatient_16,
                                 eresponse_05_col = eresponse_05,
                                 esituation_02_col = esituation_02,
                                 evitals_06_col = evitals_06,
                                 evitals_14_col = evitals_14,
                                 evitals_23_col = evitals_23,
                                 transport_disposition_col = edisposition_30

  )

  testthat::expect_equal(nrow(result$filter_process), 8)
  testthat::expect_true(is.list(result))

})
