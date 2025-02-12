testthat::test_that("hypoglycemia_01_population rejects invalid argument combinations", {

  testthat::expect_error(hypoglycemia_01_population(df = tibble::tibble(),
                                         patient_scene_table = tibble::tibble(),
                                         response_table = tibble::tibble(),
                                         situation_table = tibble::tibble(),
                                         medications_table = tibble::tibble(),
                                         vitals_table = tibble::tibble(),
                                         procedures_table = tibble::tibble(),
                                         erecord_01_col = character(),
                                         epatient_15_col = numeric(),
                                         epatient_16_col = character(),
                                         eresponse_05_col = character(),
                                         esituation_11_col = character(),
                                         esituation_12_col = character(),
                                         emedications_03_col = character(),
                                         evitals_18_col = numeric(),
                                         evitals_23_col = numeric(),
                                         evitals_26_col = character(),
                                         eprocedures_03_col = character()
                                         ),
                         "Please choose to either pass an object of class"
                         )

  testthat::expect_error(hypoglycemia_01_population(patient_scene_table = list(),
                                                    response_table = tibble::tibble(),
                                                    situation_table = tibble::tibble(),
                                                    medications_table = tibble::tibble(),
                                                    vitals_table = tibble::tibble(),
                                                    procedures_table = tibble::tibble(),
                                                    erecord_01_col = character(),
                                                    epatient_15_col = numeric(),
                                                    epatient_16_col = character(),
                                                    incident_date_col = date(),
                                                    patient_DOB_col = date(),
                                                    eresponse_05_col = character(),
                                                    esituation_11_col = character(),
                                                    esituation_12_col = character(),
                                                    emedications_03_col = character(),
                                                    evitals_18_col = numeric(),
                                                    evitals_23_col = numeric(),
                                                    evitals_26_col = character(),
                                                    eprocedures_03_col = character()
                                                    ),
                         "An object of class")
})

testthat::test_that("hypoglycemia_01_population rejects missing required column arguments", {
  testthat::expect_error(hypoglycemia_01_population(df = tibble::tibble(), epatient_15_col = "Age"),
                         "One or more of the \\*_col arguments is missing")
})

testthat::test_that("hypoglycemia_01_population rejects non-dataframe inputs", {
  testthat::expect_error(hypoglycemia_01_population(df = list()),
                         "One or more")

  testthat::expect_error(hypoglycemia_01_population(patient_scene_table = matrix()),
                         "One or more")
})

testthat::test_that("hypoglycemia_01_population validates date column formats", {

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
                       emedications_03_col = character(),
                       evitals_18_col = numeric(),
                       evitals_23_col = numeric(),
                       evitals_26_col = character(),
                       eprocedures_03_col = character()
  )

  testthat::expect_error(
    hypoglycemia_01_population(
      df,
      erecord_01_col = erecord_01,
      incident_date_col = incident_date,
      patient_DOB_col = patient_dob,
      epatient_15_col = epatient_15,
      epatient_16_col = epatient_16,
      eresponse_05_col = eresponse_05,
      esituation_11_col = esituation_11,
      esituation_12_col = esituation_12,
      emedications_03_col = emedications_03,
      evitals_18_col = evitals_18,
      evitals_23_col = evitals_23,
      evitals_26_col = evitals_26,
      eprocedures_03_col = eprocedures_03
    )
  )

  testthat::expect_no_success(
    hypoglycemia_01_population(
      patient_scene_table = tibble::tibble(),
      response_table = tibble::tibble(),
      situation_table = tibble::tibble(),
      medications_table = tibble::tibble(),
      erecord_01_col = character(),
      incident_date_col = "stuff",
      patient_DOB_col = "stuff",
      epatient_15_col = numeric(),
      epatient_16_col = character(),
      eresponse_05_col = character(),
      esituation_11_col = character(),
      esituation_12_col = character(),
      emedications_03_col = character(),
      evitals_18_col = numeric(),
      evitals_23_col = numeric(),
      evitals_26_col = character(),
      eprocedures_03_col = character()
    )
  )

})

testthat::test_that("hypoglycemia_01_population fails with unknown columns", {
  df <- tibble::tibble(
    erecord_01 = 1:3,
    incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01")),
    patient_dob = as.Date(c("2000-01-01", "2020-01-01", "2023-01-01")),
    epatient_15 = c(25, 5, 2),
    epatient_16 = c("years", "years", "months"),
    eresponse_05 = c("911", "911", "911"),
    esituation_11 = c("weakness", "asthma", "bronchospasm"),
    esituation_12 = c("asthma", "weakness", "weakness"),
    emedications_03 = c("albuterol", "levalbuterol", "metaproterenol"),
    evitals_18 = c(60, 59, 58),
    evitals_23 = c(16, 15, 14),
    evitals_26 = c("Alert", "Painful", "Unresponsive"),
    eprocedures_03 = rep("710925007", 3)
  )

  testthat::expect_error(hypoglycemia_01_population(df,
                                                       erecord_01_col = erecord_01,
                                                       incident_date_col = incident_date,
                                                       patient_DOB_col = patient_dob,
                                                       epatient_15_col = epatient_15,
                                                       epatient_16_col = epatient_16,
                                                    eresponse_05_col = character(),
                                                    esituation_11_col = "dummy",
                                                    esituation_12_col = "dummy",
                                                    emedications_03_col = "dummy",
                                                    evitals_18_col = "dummy",
                                                    evitals_23_col = "dummy",
                                                    evitals_26_col = "dummy",
                                                    eprocedures_03_col = "dummy"), "exist")

})


testthat::test_that("hypoglycemia_01_population correctly classifies patient age", {
  df <- tibble::tibble(
    erecord_01 = 1:3,
    incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01")),
    patient_dob = as.Date(c("2000-01-01", "2020-01-01", "2023-01-01")),
    epatient_15 = c(25, 5, 2),
    epatient_16 = c("years", "years", "months"),
    eresponse_05 = c("911", "911", "911"),
    esituation_11 = c("weakness", "asthma", "bronchospasm"),
    esituation_12 = c("asthma", "weakness", "weakness"),
    emedications_03 = c("albuterol", "levalbuterol", "metaproterenol"),
    evitals_18 = c(60, 59, 58),
    evitals_23 = c(16, 15, 14),
    evitals_26 = c("Alert", "Painful", "Unresponsive"),
    eprocedures_03 = rep("710925007", 3)
  )

  result <- hypoglycemia_01_population(df,
                                          erecord_01_col = erecord_01,
                                          incident_date_col = incident_date,
                                          patient_DOB_col = patient_dob,
                                          epatient_15_col = epatient_15,
                                          epatient_16_col = epatient_16,
                                       eresponse_05_col = eresponse_05,
                                       esituation_11_col = esituation_11,
                                       esituation_12_col = esituation_12,
                                       emedications_03_col = emedications_03,
                                       evitals_18_col = evitals_18,
                                       evitals_23_col = evitals_23,
                                       evitals_26_col = evitals_26,
                                       eprocedures_03_col = eprocedures_03
  )

  testthat::expect_true(all(result$adults$system_age_adult == TRUE))
  testthat::expect_true(all(result$adults$system_age_minor == FALSE))
})

testthat::test_that("hypoglycemia_01_population correctly filters 911 calls", {
  df <- tibble::tibble(
    erecord_01 = 1:3,
    incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01")),
    patient_dob = as.Date(c("2000-01-01", "2020-01-01", "2023-01-01")),
    epatient_15 = c(25, 5, 2),
    epatient_16 = c("years", "years", "months"),
    eresponse_05 = c("2205001", "2205009", "2205003"),
    esituation_11 = c("weakness", "asthma", "bronchospasm"),
    esituation_12 = c("asthma", "weakness", "weakness"),
    emedications_03 = c("albuterol", "levalbuterol", "metaproterenol"),
    evitals_18 = c(60, 59, 58),
    evitals_23 = c(16, 15, 14),
    evitals_26 = c("Alert", "Painful", "Unresponsive"),
    eprocedures_03 = rep("710925007", 3)
  )

  result <- hypoglycemia_01_population(df,
                                          erecord_01_col = erecord_01,
                                          incident_date_col = incident_date,
                                          patient_DOB_col = patient_dob,
                                          epatient_15_col = epatient_15,
                                          epatient_16_col = epatient_16,
                                       eresponse_05_col = eresponse_05,
                                       esituation_11_col = esituation_11,
                                       esituation_12_col = esituation_12,
                                       emedications_03_col = emedications_03,
                                       evitals_18_col = evitals_18,
                                       evitals_23_col = evitals_23,
                                       evitals_26_col = evitals_26,
                                       eprocedures_03_col = eprocedures_03
  )

  emergency_calls <- result$filter_process |>
    dplyr::filter(filter == "911 calls") |>
    dplyr::pull(count)

  testthat::expect_equal(nrow(result$filter_process), 7)
  testthat::expect_equal(emergency_calls, 3)

})

testthat::test_that("hypoglycemia_01_population runs correctly with table inputs", {

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
    esituation_11 = c(rep("E13.64", 3), rep("E16.2", 2)),
    esituation_12 = c(rep("E13.64", 2), rep("E16.2", 3))
  )

  medications_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    emedications_03 = c("Albuterol", "Albuterol", "Epinephrine", "None", "Albuterol")

  )

  vitals_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    evitals_18 = c(60, 59, 58, 57, 56),
    evitals_23 = c(16, 15, 14, 13, 12),
    evitals_26 = c("Alert", "Painful", "Verbal", "Unresponsive", "Alert")

  )

  procedures_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    eprocedures_03 = rep("710925007", 5)

  )

  # test the success of the function

  result <- hypoglycemia_01_population(patient_scene_table = patient_table,
                              response_table = response_table,
                              situation_table = situation_table,
                              medications_table = medications_table,
                              vitals_table = vitals_table,
                              procedures_table = procedures_table,
                              erecord_01_col = erecord_01,
                              incident_date_col = incident_date,
                              patient_DOB_col = patient_dob,
                              epatient_15_col = epatient_15,
                              epatient_16_col = epatient_16,
                              eresponse_05_col = eresponse_05,
                              esituation_11_col = esituation_11,
                              esituation_12_col = esituation_12,
                              emedications_03_col = emedications_03,
                              evitals_18_col = evitals_18,
                              evitals_23_col = evitals_23,
                              evitals_26_col = evitals_26,
                              eprocedures_03_col = eprocedures_03
                              )

  testthat::expect_equal(nrow(result$filter_process), 7)
  testthat::expect_true(is.list(result))

})
