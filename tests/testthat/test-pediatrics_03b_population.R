testthat::test_that("pediatrics_03b_population rejects invalid argument combinations", {

  testthat::expect_error(pediatrics_03b_population(df = tibble::tibble(),
                                                   patient_scene_table = tibble::tibble(),
                                                   response_table = tibble::tibble(),
                                                   exam_table = tibble::tibble(),
                                                   medications_table = tibble::tibble(),
                                                   erecord_01_col = character(),
                                                   incident_date_col = date(),
                                                   patient_DOB_col = date(),
                                                   epatient_15_col = numeric(),
                                                   epatient_16_col = character(),
                                                   eresponse_05_col = character(),
                                                   emedications_03_col = character(),
                                                   emedications_04_col = character,
                                                   eexam_01_col = character(),
                                                   eexam_02_col = character()
  ),
  "Please choose to either pass an object of class"
  )

  testthat::expect_error(pediatrics_03b_population(patient_scene_table = list(),
                                                   response_table = tibble::tibble(),
                                                   exam_table = tibble::tibble(),
                                                   medications_table = tibble::tibble(),
                                                   erecord_01_col = character(),
                                                   incident_date_col = date(),
                                                   patient_DOB_col = date(),
                                                   epatient_15_col = numeric(),
                                                   epatient_16_col = character(),
                                                   eresponse_05_col = character(),
                                                   emedications_03_col = character(),
                                                   emedications_04_col = character,
                                                   eexam_01_col = character(),
                                                   eexam_02_col = character()
  ),
  "An object of class")
})

testthat::test_that("pediatrics_03b_population rejects missing required column arguments", {
  testthat::expect_error(pediatrics_03b_population(df = tibble::tibble(), epatient_15_col = "Age"),
                         "One or more of the \\*_col arguments is missing")
})

testthat::test_that("pediatrics_03b_population rejects non-dataframe inputs", {
  testthat::expect_error(pediatrics_03b_population(df = list()),
                         "One or more")

  testthat::expect_error(pediatrics_03b_population(patient_scene_table = matrix()),
                         "One or more")
})

testthat::test_that("pediatrics_03b_population validates date column formats", {

  df <- tibble::tibble(erecord_01 = character(),
                       incident_date = as.character(Sys.Date()),
                       patient_dob = as.character(Sys.Date() - 365),
                       epatient_15 = numeric(),
                       epatient_16 = character(),
                       eresponse_05 = character(),
                       emedications_03 = character(),
                       emedications_04 = character(),
                       eexam_01 = character(),
                       eexam_02 = character()
  )

  testthat::expect_error(
    pediatrics_03b_population(
      df,
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
  )

  testthat::expect_error(
    pediatrics_03b_population(
      patient_scene_table = tibble::tibble(),
      response_table = tibble::tibble(),
      exam_table = tibble::tibble(),
      medications_table = tibble::tibble(),
      erecord_01_col = character(),
      incident_date_col = "stuff",
      patient_DOB_col = "stuff",
      epatient_15_col = numeric(),
      epatient_16_col = character(),
      eresponse_05_col = character(),
      emedications_03_col = character(),
      emedications_04_col = character(),
      eexam_01_col = character(),
      eexam_02_col = character()
    )
  )

})

testthat::test_that("pediatrics_03b_population fails with unknown columns", {
  df <- tibble::tibble(
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

  testthat::expect_error(pediatrics_03b_population(df,
                                                   erecord_01_col = erecord_01,
                                                   incident_date_col = incident_date,
                                                   patient_DOB_col = patient_dob,
                                                   epatient_15_col = epatient_15,
                                                   epatient_16_col = epatient_16,
                                                   eresponse_05_col = "DUMMY",
                                                   emedications_03_col = "DUMMY",
                                                   emedications_04_col = "DUMMY",
                                                   eexam_01_col = "DUMMY",
                                                   eexam_02_col = "DUMMY"
                                                   ),
                         "exist"
                         )

})


testthat::test_that("pediatrics_03b_population correctly classifies patient age", {
  df <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(45, 55, 35, 25, 65),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Years", "Years"),
    eresponse_05 = rep(2205001, 5),
    emedications_03 = rep("stuff", 5),
    emedications_04 = c("Inhalation", "pill", "liquid", "pill", "liquid"),
    eexam_01 = c(60, 59, 58, 57, 56),
    eexam_02 = c("Red", "Purple", "Grey", "Yellow", "Orange")
  )

  result <- pediatrics_03b_population(df,
                                      erecord_01_col = erecord_01,
                                      incident_date_col = NULL,
                                      patient_DOB_col = NULL,
                                      epatient_15_col = epatient_15,
                                      epatient_16_col = epatient_16,
                                      eresponse_05_col = eresponse_05,
                                      emedications_03_col = emedications_03,
                                      emedications_04_col = emedications_04,
                                      eexam_01_col = eexam_01,
                                      eexam_02_col = eexam_02
  )

  testthat::expect_true(all(result$adults$system_age_adult == TRUE))
  testthat::expect_true(all(result$adults$system_age_minor == FALSE))
})

testthat::test_that("pediatrics_03b_population correctly filters 911 calls", {
  df <- tibble::tibble(
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

  result <- pediatrics_03b_population(df,
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

  emergency_calls <- result$filter_process |>
    dplyr::filter(filter == "911 calls") |>
    dplyr::pull(count)

  testthat::expect_equal(nrow(result$filter_process), 6)
  testthat::expect_equal(emergency_calls, 5)

})

testthat::test_that("pediatrics_03b_population runs correctly with table inputs", {

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

  result <- pediatrics_03b_population(patient_scene_table = patient_table,
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

  testthat::expect_equal(nrow(result$filter_process), 6)
  testthat::expect_true(is.list(result))

})
