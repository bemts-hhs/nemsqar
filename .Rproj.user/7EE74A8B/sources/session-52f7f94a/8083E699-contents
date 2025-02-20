testthat::test_that("ttr_01_population rejects invalid argument combinations", {

  testthat::expect_error(ttr_01_population(df = tibble::tibble(),
                                              patient_scene_table = tibble::tibble(),
                                              response_table = tibble::tibble(),
                                              arrest_table = tibble::tibble(),
                                              vitals_table = tibble::tibble(),
                                              disposition_table = tibble::tibble(),
                                              erecord_01_col = character(),
                                              epatient_15_col = numeric(),
                                              epatient_16_col = character(),
                                              eresponse_05_col = character(),
                                              earrest_01_col = character(),
                                              evitals_06_col = numeric(),
                                              evitals_07_col = numeric(),
                                              evitals_10_col = numeric(),
                                              evitals_12_col = numeric(),
                                              evitals_14_col = numeric(),
                                              evitals_23_col = character(),
                                              evitals_26_col = character(),
                                              transport_disposition_col = character()
  ),
  "requires either a "
  )

  testthat::expect_error(ttr_01_population(patient_scene_table = list(),
                                           response_table = tibble::tibble(),
                                           arrest_table = tibble::tibble(),
                                           vitals_table = tibble::tibble(),
                                           disposition_table = tibble::tibble(),
                                           erecord_01_col = character(),
                                           epatient_15_col = numeric(),
                                           epatient_16_col = character(),
                                           eresponse_05_col = character(),
                                           earrest_01_col = character(),
                                           evitals_06_col = numeric(),
                                           evitals_07_col = numeric(),
                                           evitals_10_col = numeric(),
                                           evitals_12_col = numeric(),
                                           evitals_14_col = numeric(),
                                           evitals_23_col = character(),
                                           evitals_26_col = character(),
                                           transport_disposition_col = character()
                                           ),
                         "Please ensure you pass an unquoted column to each of the"
                         )

})

testthat::test_that("ttr_01_population rejects missing required column arguments", {
  testthat::expect_error(ttr_01_population(df = tibble::tibble(), epatient_15_col = "Age"),
                         "One or more of the \\*_col arguments is missing")
})

testthat::test_that("ttr_01_population rejects non-dataframe inputs", {
  testthat::expect_error(ttr_01_population(df = list()),
                         "One or more")

  testthat::expect_error(ttr_01_population(patient_scene_table = matrix()),
                         "One or more")
})

testthat::test_that("ttr_01_population validates date column formats", {

  df <- tibble::tibble(erecord_01_col = character(),
                       incident_date = character(),
                       patient_dob = character(),
                       epatient_15_col = numeric(),
                       epatient_16_col = character(),
                       eresponse_05_col = character(),
                       earrest_01_col = character(),
                       evitals_06_col = numeric(),
                       evitals_07_col = numeric(),
                       evitals_10_col = numeric(),
                       evitals_12_col = numeric(),
                       evitals_14_col = numeric(),
                       evitals_23_col = character(),
                       evitals_26_col = character(),
                       transport_disposition_col = character()
                       )

  testthat::expect_error(
    ttr_01_population(
      df,
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
  )

  testthat::expect_error(
    ttr_01_population(
      patient_scene_table = tibble::tibble(),
      response_table = tibble::tibble(),
      arrest_table = tibble::tibble(),
      vitals_table = tibble::tibble(),
      disposition_table = tibble::tibble(),
      erecord_01_col = character(),
      incident_date_col = character(),
      patient_DOB_col = character(),
      epatient_15_col = numeric(),
      epatient_16_col = character(),
      eresponse_05_col = character(),
      earrest_01_col = character(),
      evitals_06_col = numeric(),
      evitals_07_col = numeric(),
      evitals_10_col = numeric(),
      evitals_12_col = numeric(),
      evitals_14_col = numeric(),
      evitals_23_col = character(),
      evitals_26_col = character(),
      transport_disposition_col = character()
    )
  )

})

testthat::test_that("ttr_01_population fails with unknown columns", {

  # Synthetic test data
  test_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01", "2025-01-01", "2025-06-01")),
    patient_dob = as.Date(c("2000-01-01", "2020-01-01", "2023-02-01", "2023-01-01", "1970-06-01")),
    epatient_15 = c(25, 5, 2, 2, 55),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Years", "Years"),
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

  testthat::expect_error(ttr_01_population(test_data,
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
                                           transport_disposition_col = "dummy"
                                           ),
                         "exist"
                         )

})


testthat::test_that("ttr_01_population correctly classifies patient age", {

  # Synthetic test data
  test_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01", "2025-01-01", "2025-06-01")),
    patient_dob = as.Date(c("2000-01-01", "2020-01-01", "2023-02-01", "2023-01-01", "1970-06-01")),
    epatient_15 = c(25, 5, 2, 2, 55),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Years", "Years"),
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

  result <- ttr_01_population(test_data,
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

  testthat::expect_true(all(result$adults$system_age_adult == TRUE))
  testthat::expect_true(all(result$adults$system_age_minor == FALSE))

  result <- ttr_01_population(test_data,
                              erecord_01_col = erecord_01,
                              incident_date_col = NULL,
                              patient_DOB_col = NULL,
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

  result <- ttr_01_population(patient_scene_table = patient_table,
                                 response_table = response_table,
                                 arrest_table = arrest_table,
                                 vitals_table = vitals_table,
                                 disposition_table = disposition_table,
                                 erecord_01_col = erecord_01,
                                 incident_date_col = NULL,
                                 patient_DOB_col = NULL,
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

  testthat::expect_true(all(result$adults$system_age_adult == TRUE))
  testthat::expect_true(all(result$adults$system_age_minor == FALSE))

  result <- ttr_01_population(patient_scene_table = patient_table,
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

  testthat::expect_true(all(result$adults$system_age_adult == TRUE))
  testthat::expect_true(all(result$adults$system_age_minor == FALSE))

})

testthat::test_that("ttr_01_population correctly filters 911 calls", {

  # Synthetic test data
  test_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01", "2025-01-01", "2025-06-01")),
    patient_dob = as.Date(c("2000-01-01", "2020-01-01", "2023-02-01", "2023-01-01", "1970-06-01")),
    epatient_15 = c(25, 5, 2, 2, 55),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Years", "Years"),
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

  result <- ttr_01_population(test_data,
                                 erecord_01_col = erecord_01,
                                 incident_date_col = NULL,
                                 patient_DOB_col = NULL,
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

  emergency_calls <- result$filter_process |>
    dplyr::filter(filter == "911 calls") |>
    dplyr::pull(count)

  testthat::expect_equal(nrow(result$filter_process), 8)
  testthat::expect_equal(emergency_calls, 5)

})

testthat::test_that("ttr_01_population runs correctly with table inputs", {

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
  result <- ttr_01_population(patient_scene_table = patient_table,
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

  testthat::expect_equal(nrow(result$filter_process), 8)
  testthat::expect_true(is.list(result))

  result <- ttr_01_population(patient_scene_table = patient_table,
                                 response_table = response_table,
                                 arrest_table = arrest_table,
                                 vitals_table = vitals_table,
                                 disposition_table = disposition_table,
                                 erecord_01_col = erecord_01,
                                 incident_date_col = NULL,
                                 patient_DOB_col = NULL,
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

  testthat::expect_equal(nrow(result$filter_process), 8)
  testthat::expect_true(is.list(result))

})
