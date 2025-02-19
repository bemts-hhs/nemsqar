testthat::test_that("respiratory_01_population rejects invalid argument combinations", {

  testthat::expect_error(respiratory_01_population(df = tibble::tibble(),
                                                    patient_scene_table = tibble::tibble(),
                                                    response_table = tibble::tibble(),
                                                    situation_table = tibble::tibble(),
                                                    vitals_table = tibble::tibble(),
                                                    erecord_01_col = character(),
                                                    epatient_15_col = numeric(),
                                                    epatient_16_col = character(),
                                                    eresponse_05_col = character(),
                                                    esituation_11_col = character(),
                                                    esituation_12_col = character(),
                                                    evitals_12_col = numeric(),
                                                    evitals_14_col = numeric(),
  ),
  "Please choose to either pass an object of class"
  )

  testthat::expect_error(respiratory_01_population(patient_scene_table = list(),
                                                    response_table = tibble::tibble(),
                                                    situation_table = tibble::tibble(),
                                                    vitals_table = tibble::tibble(),
                                                    erecord_01_col = character(),
                                                    epatient_15_col = numeric(),
                                                    epatient_16_col = character(),
                                                    incident_date_col = date(),
                                                    patient_DOB_col = date(),
                                                    eresponse_05_col = character(),
                                                    esituation_11_col = character(),
                                                    esituation_12_col = character(),
                                                    evitals_12_col = numeric(),
                                                    evitals_14_col = numeric(),
  ),
  "An object of class")
})

testthat::test_that("respiratory_01_population rejects missing required column arguments", {
  testthat::expect_error(respiratory_01_population(df = tibble::tibble(), epatient_15_col = "Age"),
                         "One or more of the \\*_col arguments is missing")
})

testthat::test_that("respiratory_01_population rejects non-dataframe inputs", {
  testthat::expect_error(respiratory_01_population(df = list()),
                         "One or more")

  testthat::expect_error(respiratory_01_population(patient_scene_table = matrix()),
                         "One or more")
})

testthat::test_that("respiratory_01_population validates date column formats", {

  df <- tibble::tibble(erecord_01 = character(),
                       incident_date = as.character(Sys.Date()),
                       patient_dob = as.character(Sys.Date() - 365),
                       epatient_15 = numeric(),
                       epatient_16 = character(),
                       incident_date_col = date(),
                       patient_DOB_col = date(),
                       eresponse_05_col = character(),
                       esituation_11_col = character(),
                       esituation_12_col = character(),
                       evitals_12_col = numeric(),
                       evitals_14_col = numeric(),
  )

  testthat::expect_error(
    respiratory_01_population(
      df,
      erecord_01_col = erecord_01,
      incident_date_col = incident_date,
      patient_DOB_col = patient_dob,
      epatient_15_col = epatient_15,
      epatient_16_col = epatient_16,
      eresponse_05_col = eresponse_05,
      esituation_11_col = esituation_11,
      esituation_12_col = esituation_12,
      evitals_12_col = evitals_18,
      evitals_14_col = evitals_23,
    )
  )

  testthat::expect_no_success(
    respiratory_01_population(
      patient_scene_table = tibble::tibble(),
      response_table = tibble::tibble(),
      situation_table = tibble::tibble(),
      erecord_01_col = character(),
      incident_date_col = "stuff",
      patient_DOB_col = "stuff",
      epatient_15_col = numeric(),
      epatient_16_col = character(),
      eresponse_05_col = character(),
      esituation_11_col = character(),
      esituation_12_col = character(),
      evitals_12_col = numeric(),
      evitals_14_col = numeric(),
    )
  )

})

testthat::test_that("respiratory_01_population fails with unknown columns", {
  df <- tibble::tibble(
    erecord_01 = 1:3,
    incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01")),
    patient_dob = as.Date(c("2000-01-01", "2020-01-01", "2023-01-01")),
    epatient_15 = c(25, 5, 2),
    epatient_16 = c("years", "years", "months"),
    eresponse_05 = c("911", "911", "911"),
    esituation_11 = rep("J80", 3),
    esituation_12 = rep("I50.9", 3),
    evitals_12 = c(60, 59, 58),
    evitals_14 = c(16, 15, 14)
  )

  testthat::expect_error(respiratory_01_population(df,
                                                    erecord_01_col = erecord_01,
                                                    incident_date_col = incident_date,
                                                    patient_DOB_col = patient_dob,
                                                    epatient_15_col = epatient_15,
                                                    epatient_16_col = epatient_16,
                                                    eresponse_05_col = character(),
                                                    esituation_11_col = "dummy",
                                                    esituation_12_col = "dummy",
                                                    evitals_12_col = "dummy",
                                                    evitals_14_col = "dummy",
                                                   ),
                         "exist"
                         )

})


testthat::test_that("respiratory_01_population correctly classifies patient age", {
  df <- tibble::tibble(
    erecord_01 = 1:3,
    incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01")),
    patient_dob = as.Date(c("2000-01-01", "2020-01-01", "2023-01-01")),
    epatient_15 = c(25, 5, 2),
    epatient_16 = c("years", "years", "months"),
    eresponse_05 = c("911", "911", "911"),
    esituation_11 = rep("J80", 3),
    esituation_12 = rep("I50.9", 3),
    evitals_12 = c(60, 59, 58),
    evitals_14 = c(16, 15, 14)
  )

  result <- respiratory_01_population(df,
                                       erecord_01_col = erecord_01,
                                       incident_date_col = incident_date,
                                       patient_DOB_col = patient_dob,
                                       epatient_15_col = epatient_15,
                                       epatient_16_col = epatient_16,
                                       eresponse_05_col = eresponse_05,
                                       esituation_11_col = esituation_11,
                                       esituation_12_col = esituation_12,
                                       evitals_12_col = evitals_12,
                                       evitals_14_col = evitals_14,
  )

  testthat::expect_true(all(result$adults$system_age_adult == TRUE))
  testthat::expect_true(all(result$adults$system_age_minor == FALSE))
})

testthat::test_that("respiratory_01_population correctly filters 911 calls", {
  df <- tibble::tibble(
    erecord_01 = 1:3,
    incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01")),
    patient_dob = as.Date(c("2000-01-01", "2020-01-01", "2023-01-01")),
    epatient_15 = c(25, 5, 2),
    epatient_16 = c("years", "years", "months"),
    eresponse_05 = rep(2205001, 3),
    esituation_11 = rep("J80", 3),
    esituation_12 = rep("I50.9", 3),
    evitals_12 = c(60, 59, 58),
    evitals_14 = c(16, 15, 14)
  )

  result <- respiratory_01_population(df,
                                       erecord_01_col = erecord_01,
                                       incident_date_col = incident_date,
                                       patient_DOB_col = patient_dob,
                                       epatient_15_col = epatient_15,
                                       epatient_16_col = epatient_16,
                                       eresponse_05_col = eresponse_05,
                                       esituation_11_col = esituation_11,
                                       esituation_12_col = esituation_12,
                                       evitals_12_col = evitals_12,
                                       evitals_14_col = evitals_14
  )

  emergency_calls <- result$filter_process |>
    dplyr::filter(filter == "911 calls") |>
    dplyr::pull(count)

  testthat::expect_equal(nrow(result$filter_process), 7)
  testthat::expect_equal(emergency_calls, 3)

})

testthat::test_that("respiratory_01_population runs correctly with table inputs", {

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
    esituation_11 = c(rep("J80", 3), rep("I50.9", 2)),
    esituation_12 = c(rep("J80", 2), rep("I50.9", 3))
  )

  vitals_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    evitals_12 = c(60, 59, 58, 57, 56),
    evitals_14 = c(16, 15, 14, 13, 12)

  )

  # test the success of the function

  result <- respiratory_01_population(patient_scene_table = patient_table,
                                       response_table = response_table,
                                       situation_table = situation_table,
                                       vitals_table = vitals_table,
                                       erecord_01_col = erecord_01,
                                       incident_date_col = incident_date,
                                       patient_DOB_col = patient_dob,
                                       epatient_15_col = epatient_15,
                                       epatient_16_col = epatient_16,
                                       eresponse_05_col = eresponse_05,
                                       esituation_11_col = esituation_11,
                                       esituation_12_col = esituation_12,
                                       evitals_12_col = evitals_12,
                                       evitals_14_col = evitals_14,
  )

  testthat::expect_equal(nrow(result$filter_process), 7)
  testthat::expect_true(is.list(result))

})
