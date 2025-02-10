testthat::test_that("safety_04_population rejects invalid argument combinations", {

  testthat::expect_error(safety_04_population(df = tibble::tibble(),
                                              patient_scene_table = tibble::tibble(),
                                              response_table = tibble::tibble(),
                                              disposition_table = tibble::tibble,
                                              procedures_table = tibble::tibble(),
                                              arrest_table = tibble::tibble(),
                                              injury_table = tibble::tibble(),
                                              erecord_01_col = character(),
                                              incident_date_col = date(),
                                              patient_DOB_col = date(),
                                              epatient_15_col = numeric(),
                                              epatient_16_col = character(),
                                              eresponse_05_col = character(),
                                              earrest_01_col = character(),
                                              einjury_03_col = character(),
                                              edisposition_14_col = character(),
                                              transport_disposition_col = character(),
                                              eprocedures_03_col = character()
  ),
  "Please choose to either pass an object of class"
  )

  testthat::expect_error(safety_04_population(patient_scene_table = list(),
                                              response_table = tibble::tibble(),
                                              disposition_table = tibble::tibble,
                                              procedures_table = tibble::tibble(),
                                              arrest_table = tibble::tibble(),
                                              injury_table = tibble::tibble(),
                                              erecord_01_col = character(),
                                              incident_date_col = date(),
                                              patient_DOB_col = date(),
                                              epatient_15_col = numeric(),
                                              epatient_16_col = character(),
                                              eresponse_05_col = character(),
                                              earrest_01_col = character(),
                                              einjury_03_col = character(),
                                              edisposition_14_col = character(),
                                              transport_disposition_col = character(),
                                              eprocedures_03_col = character()
                                              ),
                         "One or more of the tables passed to"
                         )

})

testthat::test_that("safety_04_population rejects missing required column arguments", {
  testthat::expect_error(safety_04_population(df = tibble::tibble(), epatient_15_col = "Age"),
                         "One or more of the \\*_col arguments is missing")
})

testthat::test_that("safety_04_population rejects non-dataframe inputs", {
  testthat::expect_error(safety_04_population(df = list()),
                         "One or more")

  testthat::expect_error(safety_04_population(patient_scene_table = matrix()),
                         "One or more")
})

testthat::test_that("safety_04_population validates date column formats", {

  df <- tibble::tibble(erecord_01 = character(),
                       incident_date = as.character(Sys.Date()),
                       patient_DOB = as.character(Sys.Date() - 365000),
                       epatient_15 = numeric(),
                       epatient_16 = character(),
                       eresponse_05 = character(),
                       earrest_01 = character(),
                       einjury_03 = character(),
                       edisposition_14 = character(),
                       transport_disposition = character(),
                       eprocedures_03 = character()
  )

  testthat::expect_error(
    safety_04_population(
      df,
      erecord_01_col = erecord_01,
      epatient_15_col = epatient_15,
      epatient_16_col = epatient_16,
      eresponse_05_col = eresponse_05,
      earrest_01_col = earrest_01,
      einjury_03_col = einjury_03,
      edisposition_14_col = edisposition_14,
      transport_disposition_col = edisposition_30,
      eprocedures_03_col = eprocedures_03
    )
  )

  testthat::expect_error(
    safety_04_population(
      patient_scene_table = tibble::tibble(),
      response_table = tibble::tibble(),
      disposition_table = tibble::tibble,
      procedures_table = tibble::tibble(),
      arrest_table = tibble::tibble(),
      injury_table = tibble::tibble(),
      erecord_01_col = character(),
      incident_date_col = date(),
      patient_DOB_col = date(),
      epatient_15_col = numeric(),
      epatient_16_col = character(),
      eresponse_05_col = character(),
      earrest_01_col = character(),
      einjury_03_col = character(),
      edisposition_14_col = character(),
      transport_disposition_col = character(),
      eprocedures_03_col = character()
    )
  )

})

testthat::test_that("safety_04_population fails with unknown columns", {
  df <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    earrest_01 = rep("No", 5),
    einjury_03 = rep("non-injury", 5),
    edisposition_14 = rep(4214001, 5),
    edisposition_30 = rep(4230001, 5),
    eprocedures_03 = rep("other response", 5)
  )

  testthat::expect_error(safety_04_population(df,
                                              erecord_01_col = erecord_01,
                                              epatient_15_col = epatient_15,
                                              epatient_16_col = epatient_16,
                                              eresponse_05_col = eresponse_05,
                                              earrest_01_col = earrest_01,
                                              einjury_03_col = einjury_03,
                                              edisposition_14_col = edisposition_14,
                                              transport_disposition_col = edisposition_30,
                                              eprocedures_03_col = eprocedures_03
                                              ),
                         "One or more of"
                         )

})


testthat::test_that("safety_04_population correctly classifies patient age", {

  df <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01", "2025-01-01", "2025-06-01")),
    patient_dob = as.Date(c("2000-01-01", "2020-01-01", "2023-02-01", "2023-01-01", "1970-06-01")),
    epatient_15 = c(25, 5, 2, 2, 55),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Years", "Years"),
    eresponse_05 = rep(2205001, 5),
    earrest_01 = rep("No", 5),
    einjury_03 = rep("non-injury", 5),
    edisposition_14 = rep(4214001, 5),
    edisposition_30 = rep(4230001, 5),
    eprocedures_03 = rep("other response", 5)
  )

  result <- safety_04_population(df,
                                 erecord_01_col = erecord_01,
                                 incident_date_col = incident_date,
                                 patient_DOB_col = patient_dob,
                                 epatient_15_col = epatient_15,
                                 epatient_16_col = epatient_16,
                                 eresponse_05_col = eresponse_05,
                                 earrest_01_col = earrest_01,
                                 einjury_03_col = einjury_03,
                                 edisposition_14_col = edisposition_14,
                                 transport_disposition_col = edisposition_30,
                                 eprocedures_03_col = eprocedures_03
                                 )

  testthat::expect_true(all(result$adults$system_age_adult == TRUE))
  testthat::expect_true(all(result$adults$system_age_minor == FALSE))

})

testthat::test_that("safety_04_population correctly filters longboard procedures", {

  df <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    earrest_01 = rep("No", 5),
    einjury_03 = rep("non-injury", 5),
    edisposition_14 = rep(4214001, 5),
    edisposition_30 = rep(4230001, 5),
    eprocedures_03 = rep("other response", 5)
  )

  result <- safety_04_population(df,
                                 erecord_01_col = erecord_01,
                                 incident_date_col = NULL,
                                 patient_DOB_col = NULL,
                                 epatient_15_col = epatient_15,
                                 epatient_16_col = epatient_16,
                                 eresponse_05_col = eresponse_05,
                                 earrest_01_col = earrest_01,
                                 einjury_03_col = einjury_03,
                                 edisposition_14_col = edisposition_14,
                                 transport_disposition_col = edisposition_30,
                                 eprocedures_03_col = eprocedures_03
                                 )

  longboards <- result$filter_process |>
    dplyr::filter(grepl(pattern = "long board", x = filter, ignore.case = TRUE)) |>
    dplyr::pull(count)

  testthat::expect_equal(nrow(result$filter_process), 10)
  testthat::expect_equal(longboards, 0)

})

testthat::test_that("safety_04_population runs correctly with table inputs", {

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

  disposition_table <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    edisposition_14 = rep(4214001, 5),
    edisposition_30 = rep(4230001, 5),
  )

  arrest_table <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    earrest_01 = rep("No", 5)
  )

  injury_table <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    einjury_03 = rep("non-injury", 5)
  )

  procedures_table <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    eprocedures_03 = rep("other response", 5)
  )

  # test the success of the function

  result <- safety_04_population(patient_scene_table = patient_table,
                                 response_table = response_table,
                                 disposition_table = disposition_table,
                                 arrest_table = arrest_table,
                                 procedures_table = procedures_table,
                                 injury_table = injury_table,
                                 erecord_01_col = erecord_01,
                                 incident_date_col = incident_date,
                                 patient_DOB_col = patient_dob,
                                 epatient_15_col = epatient_15,
                                 epatient_16_col = epatient_16,
                                 eresponse_05_col = eresponse_05,
                                 earrest_01_col = earrest_01,
                                 einjury_03_col = einjury_03,
                                 edisposition_14_col = edisposition_14,
                                 transport_disposition_col = edisposition_30,
                                 eprocedures_03_col = eprocedures_03

  )

  testthat::expect_equal(nrow(result$filter_process), 10)
  testthat::expect_true(is.list(result))

})
