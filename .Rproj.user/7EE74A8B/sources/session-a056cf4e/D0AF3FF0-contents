testthat::test_that("asthma_01_population rejects invalid argument combinations", {
  testthat::expect_error(nemsqar::asthma_01_population(df = tibble::tibble(), patient_scene_table = tibble::tibble()),
               "Please choose to either pass an object of class")

  testthat::expect_error(nemsqar::asthma_01_population(),
               "Please choose to either pass an object of class")
})

testthat::test_that("asthma_01_population rejects missing required column arguments", {
  testthat::expect_error(nemsqar::asthma_01_population(df = tibble::tibble(), epatient_15_col = "Age"),
               "One or more of the \\*_col arguments is missing")
})

testthat::test_that("asthma_01_population rejects non-dataframe inputs", {
  testthat::expect_error(nemsqar::asthma_01_population(df = list()),
               "One or more")

  testthat::expect_error(nemsqar::asthma_01_population(patient_scene_table = matrix()),
               "One or more")

  testthat::expect_error(
    asthma_01_population(patient_scene_table = list(),
                         response_table = tibble::tibble(),
                         situation_table = tibble::tibble(),
                         medications_table = tibble::tibble(),
                         erecord_01_col = character(),
                         incident_date_col = as.character(Sys.Date()),
                         patient_dob_col = as.character(Sys.Date() - 365),
                         epatient_15_col = numeric(),
                         epatient_16_col = character(),
                         eresponse_05_col = character(),
                         esituation_11_col = character(),
                         esituation_12_col = character(),
                         emedications_03_col = character()
                         )
  )
})

testthat::test_that("asthma_01_population validates date column formats", {
  df <- tibble::tibble(erecord_01 = character(),
                       incident_date = as.character(Sys.Date()),
                       patient_dob = as.character(Sys.Date() - 365),
                       epatient_15 = numeric(),
                       epatient_16 = character(),
                       eresponse_05 = character(),
                       esituation_11 = character(),
                       esituation_12 = character(),
                       emedications_03 = character()
                       )

  testthat::expect_error(nemsqar::asthma_01_population(df,
                                                       erecord_01_col = erecord_01,
                                                       incident_date_col = incident_date,
                                                       patient_dob_col = patient_dob,
                                                       epatient_15_col = epatient_15,
                                                       epatient_16_col = epatient_16,
                                                       eresponse_05_col = eresponse_05,
                                                       esituation_11_col = esituation_11,
                                                       esituation_12_col = esituation_12,
                                                       emedications_03_col = emedications_03
                                                       ), "one or both")

  testthat::expect_error(
    asthma_01_population(patient_scene_table = tibble::tibble(),
                         response_table = tibble::tibble(),
                         situation_table = tibble::tibble(),
                         medications_table = tibble::tibble(),
                         erecord_01_col = character(),
                         incident_date_col = "stuff",
                         patient_dob_col = "stuff",
                         epatient_15_col = numeric(),
                         epatient_16_col = character(),
                         eresponse_05_col = character(),
                         esituation_11_col = character(),
                         esituation_12_col = character(),
                         emedications_03_col = character()
                         ),
    regexp = "one or both"
  )

})

testthat::test_that("asthma_01_population fails with unknown columns", {
  df <- tibble::tibble(
    erecord_01 = 1:3,
    incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01")),
    patient_dob = as.Date(c("2000-01-01", "2020-01-01", "2023-01-01")),
    epatient_15 = c(25, 5, 2),
    epatient_16 = c("years", "years", "months"),
    eresponse_05 = c("911", "911", "911"),
    esituation_11 = c("weakness", "asthma", "bronchospasm"),
    esituation_12 = c("asthma", "weakness", "weakness"),
    emedications_03 = c("albuterol", "levalbuterol", "metaproterenol")
  )

  testthat::expect_error(nemsqar::asthma_01_population(df,
                                          erecord_01_col = erecord_01,
                                          incident_date_col = incident_date,
                                          patient_dob_col = patient_dob,
                                          epatient_15_col = epatient_15,
                                          epatient_16_col = epatient_16,
                                          eresponse_05_col = "dummy",
                                          esituation_11_col = "dummy",
                                          esituation_12_col = "dummy",
                                          emedications_03_col = "dummy"), "exist")

})


testthat::test_that("asthma_01_population correctly classifies patient age", {
  df <- tibble::tibble(
    erecord_01 = 1:3,
    incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01")),
    patient_dob = as.Date(c("2000-01-01", "2020-01-01", "2023-01-01")),
    epatient_15 = c(25, 5, 2),
    epatient_16 = c("years", "years", "months"),
    eresponse_05 = c("911", "911", "911"),
    esituation_11 = c("weakness", "asthma", "bronchospasm"),
    esituation_12 = c("asthma", "weakness", "weakness"),
    emedications_03 = c("albuterol", "levalbuterol", "metaproterenol")
  )

  result <- nemsqar::asthma_01_population(df,
                                          erecord_01_col = erecord_01,
                                          incident_date_col = incident_date,
                                          patient_dob_col = patient_dob,
                                          epatient_15_col = epatient_15,
                                          epatient_16_col = epatient_16,
                                          eresponse_05_col = eresponse_05,
                                          esituation_11_col = esituation_11,
                                          esituation_12_col = esituation_12,
                                          emedications_03_col = emedications_03
                                          )

  testthat::expect_true(all(result$adults$system_age_adult == TRUE))
  testthat::expect_true(all(result$adults$system_age_minor == FALSE))
})

testthat::test_that("asthma_01_population correctly filters 911 calls", {
  df <- tibble::tibble(
    erecord_01 = 1:3,
    incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01")),
    patient_dob = as.Date(c("2000-01-01", "2020-01-01", "2023-01-01")),
    epatient_15 = c(25, 5, 2),
    epatient_16 = c("years", "years", "months"),
    eresponse_05 = c("2205001", "2205009", "2205003"),
    esituation_11 = c("weakness", "asthma", "bronchospasm"),
    esituation_12 = c("asthma", "weakness", "weakness"),
    emedications_03 = c("albuterol", "levalbuterol", "metaproterenol")
  )

  result <- nemsqar::asthma_01_population(df,
                                          erecord_01_col = erecord_01,
                                          incident_date_col = incident_date,
                                          patient_dob_col = patient_dob,
                                          epatient_15_col = epatient_15,
                                          epatient_16_col = epatient_16,
                                          eresponse_05_col = eresponse_05,
                                          esituation_11_col = esituation_11,
                                          esituation_12_col = esituation_12,
                                          emedications_03_col = emedications_03
                                          )

  emergency_calls <- result$filter_process |>
    dplyr::filter(filter == "911 calls") |>
    dplyr::pull(count)

  testthat::expect_equal(nrow(result$filter_process), 7)
  testthat::expect_equal(emergency_calls, 3)

})

testthat::test_that("asthma_01_population runs correctly with table inputs", {

  # create tables to test correct functioning
  patient_table <- tibble::tibble(

  erecord_01 = 1:3,
  incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01")),
  patient_dob = as.Date(c("2000-01-01", "2020-01-01", "2023-01-01")),
  epatient_15 = c(25, 5, 2),
  epatient_16 = c("years", "years", "months")

  )

  response_table <- tibble::tibble(

  erecord_01 = 1:3,
  eresponse_05 = c("2205001", "2205009", "2205003")

  )

  situation_table <- tibble::tibble(

  erecord_01 = 1:3,
  esituation_11 = c("weakness", "asthma", "bronchospasm"),
  esituation_12 = c("asthma", "weakness", "weakness")
  )

  medications_table <- tibble::tibble(

  erecord_01 = 1:3,
  emedications_03 = c("albuterol", "levalbuterol", "metaproterenol")

  )

  # test the success of the function

  result <- asthma_01_population(patient_scene_table = patient_table,
                                                response_table = response_table,
                                                situation_table = situation_table,
                                                medications_table = medications_table,
                                                erecord_01_col = erecord_01,
                                                incident_date_col = incident_date,
                                                patient_dob_col = patient_dob,
                                                epatient_15_col = epatient_15,
                                                epatient_16_col = epatient_16,
                                                eresponse_05_col = eresponse_05,
                                                esituation_11_col = esituation_11,
                                                esituation_12_col = esituation_12,
                                                emedications_03_col = emedications_03
                                                )

  testthat::expect_equal(nrow(result$filter_process), 7)
  testthat::expect_true(is.list(result))

})
