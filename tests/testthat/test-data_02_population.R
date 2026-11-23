testthat::test_that("data_02_population() errors when both df and tables are supplied", {
  test_data <- tibble::tibble(
    id = 1
  )

  testthat::expect_error(
    data_02_population(
      df = test_data,
      patient_scene_table = test_data,
      audit_table = test_data,
      disposition_table = test_data,
      times_table = test_data,
      erecord_01_col = id,
      incident_date_col = id,
      patient_DOB_col = id,
      epatient_15_col = id,
      epatient_16_col = id,
      patient_care_col = id,
      time_available_to_document_col = id,
      time_documentation_occurred_col = id
    )
  )
})

testthat::test_that("data_02_population() errors when no input source is supplied", {
  testthat::expect_error(
    data_02_population(
      erecord_01_col = id,
      incident_date_col = incident_date,
      patient_DOB_col = dob,
      epatient_15_col = age,
      epatient_16_col = units,
      patient_care_col = disposition,
      time_available_to_document_col = available,
      time_documentation_occurred_col = documented
    )
  )
})


# Synthetic test data ----------------------------------------------------

test_data_02 <- tibble::tibble(
  erecord_01 = paste0("R", 1:4),
  incident_date = as.Date("2024-01-01"),
  patient_dob = as.Date(
    c(
      "1980-01-01",
      "1975-01-01",
      "2015-01-01",
      "2016-01-01"
    )
  ),
  epatient_15 = c(44, 49, 9, 8),
  epatient_16 = c(
    "Years",
    "Years",
    "Years",
    "Years"
  ),
  patient_disposition = rep(
    "Patient Treated, Transported by this EMS Unit (4212033)",
    4
  ),
  unit_back_in_service = as.POSIXct(
    c(
      "2024-01-01 08:00:00",
      "2024-01-01 08:00:00",
      "2024-01-01 08:00:00",
      "2024-01-01 08:00:00"
    ),
    tz = "UTC"
  ),
  incident_created = as.POSIXct(
    c(
      "2024-01-01 11:00:00",
      "2024-01-02 08:00:00",
      "2024-01-05 08:00:00",
      "2024-04-15 08:00:00"
    ),
    tz = "UTC"
  )
)

testthat::test_that("data_02_population() returns expected output structure", {
  results <- data_02_population(
    df = test_data_02,
    erecord_01_col = erecord_01,
    incident_date_col = incident_date,
    patient_DOB_col = patient_dob,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    patient_care_col = patient_disposition,
    time_available_to_document_col = unit_back_in_service,
    time_documentation_occurred_col = incident_created
  )

  testthat::expect_named(
    results,
    c(
      "filter_process",
      "adults",
      "peds",
      "initial_population",
      "computing_population",
      "missingness"
    )
  )
})

testthat::test_that("adult and pediatric populations are generated correctly", {
  results <- data_02_population(
    df = test_data_02,
    erecord_01_col = erecord_01,
    incident_date_col = incident_date,
    patient_DOB_col = patient_dob,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    patient_care_col = patient_disposition,
    time_available_to_document_col = unit_back_in_service,
    time_documentation_occurred_col = incident_created
  )

  testthat::expect_equal(
    nrow(results$adults),
    2
  )

  testthat::expect_equal(
    nrow(results$peds),
    2
  )
})

testthat::test_that("documentation time variables are created", {
  results <- data_02_population(
    df = test_data_02,
    erecord_01_col = erecord_01,
    incident_date_col = incident_date,
    patient_DOB_col = patient_dob,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    patient_care_col = patient_disposition,
    time_available_to_document_col = unit_back_in_service,
    time_documentation_occurred_col = incident_created
  )

  computing_population <- results$computing_population

  testthat::expect_true(
    "RECORD_COMPLETION_HOURS" %in%
      names(computing_population)
  )

  testthat::expect_true(
    "RECORD_COMPLETION_DAYS" %in%
      names(computing_population)
  )

  testthat::expect_true(
    "RECORD_COMPLETION_DAYS_LABEL" %in%
      names(computing_population)
  )
})

testthat::test_that("documentation time calculations are correct", {
  results <- data_02_population(
    df = test_data_02,
    erecord_01_col = erecord_01,
    incident_date_col = incident_date,
    patient_DOB_col = patient_dob,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    patient_care_col = patient_disposition,
    time_available_to_document_col = unit_back_in_service,
    time_documentation_occurred_col = incident_created
  )

  testthat::expect_equal(
    results$computing_population$RECORD_COMPLETION_HOURS[1],
    3
  )

  testthat::expect_equal(
    results$computing_population$RECORD_COMPLETION_DAYS[1],
    3 / 24
  )
})

testthat::test_that("chronology flag identifies documentation before availability", {
  chronology_data <- test_data_02

  chronology_data$incident_created[1] <-
    chronology_data$unit_back_in_service[1] - 3600

  results <- data_02_population(
    df = chronology_data,
    erecord_01_col = erecord_01,
    incident_date_col = incident_date,
    patient_DOB_col = patient_dob,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    patient_care_col = patient_disposition,
    time_available_to_document_col = unit_back_in_service,
    time_documentation_occurred_col = incident_created
  )

  testthat::expect_true(
    results$computing_population$DOCUMENTATION_OCCURRED_BEFORE_AVAILABLE[1]
  )
})

testthat::test_that("all treated patients remain in initial population", {
  results <- data_02_population(
    df = test_data_02,
    erecord_01_col = erecord_01,
    incident_date_col = incident_date,
    patient_DOB_col = patient_dob,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    patient_care_col = patient_disposition,
    time_available_to_document_col = unit_back_in_service,
    time_documentation_occurred_col = incident_created
  )

  testthat::expect_equal(
    nrow(results$initial_population),
    4
  )
})

testthat::test_that("filter_process contains expected rows", {
  results <- data_02_population(
    df = test_data_02,
    erecord_01_col = erecord_01,
    incident_date_col = incident_date,
    patient_DOB_col = patient_dob,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    patient_care_col = patient_disposition,
    time_available_to_document_col = unit_back_in_service,
    time_documentation_occurred_col = incident_created
  )

  testthat::expect_true(
    all(
      c(
        "Patients evaluated and care provided",
        "Documentation occurred before available to document",
        "Adults denominator",
        "Peds denominator",
        "Initial population",
        "Total dataset"
      ) %in%
        results$filter_process$filter
    )
  )
})

testthat::test_that("data_02_population works using separate tables", {
  patient_scene_table <- test_data_02 |>
    dplyr::select(
      erecord_01,
      incident_date,
      patient_dob,
      epatient_15,
      epatient_16
    )

  audit_table <- test_data_02 |>
    dplyr::select(
      erecord_01,
      incident_created
    )

  disposition_table <- test_data_02 |>
    dplyr::select(
      erecord_01,
      patient_disposition
    )

  times_table <- test_data_02 |>
    dplyr::select(
      erecord_01,
      unit_back_in_service
    )

  results <- data_02_population(
    patient_scene_table = patient_scene_table,
    audit_table = audit_table,
    disposition_table = disposition_table,
    times_table = times_table,
    erecord_01_col = erecord_01,
    incident_date_col = incident_date,
    patient_DOB_col = patient_dob,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    patient_care_col = patient_disposition,
    time_available_to_document_col = unit_back_in_service,
    time_documentation_occurred_col = incident_created
  )

  testthat::expect_true(
    is.list(results)
  )

  testthat::expect_named(
    results,
    c(
      "filter_process",
      "adults",
      "peds",
      "initial_population",
      "computing_population",
      "missingness"
    )
  )

  testthat::expect_equal(
    nrow(results$initial_population),
    4
  )
})

testthat::test_that("system age logic works without DOB variables", {
  results <- data_02_population(
    df = test_data_02,
    erecord_01_col = erecord_01,
    incident_date_col = NULL,
    patient_DOB_col = NULL,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    patient_care_col = patient_disposition,
    time_available_to_document_col = unit_back_in_service,
    time_documentation_occurred_col = incident_created
  )

  testthat::expect_equal(
    nrow(results$adults),
    2
  )

  testthat::expect_equal(
    nrow(results$peds),
    2
  )
})

testthat::test_that("adult and pediatric populations are created from system age fields", {
  results <- data_02_population(
    df = test_data_02,
    erecord_01_col = erecord_01,
    incident_date_col = NULL,
    patient_DOB_col = NULL,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    patient_care_col = patient_disposition,
    time_available_to_document_col = unit_back_in_service,
    time_documentation_occurred_col = incident_created
  )

  testthat::expect_true(
    all(results$adults$epatient_15 >= 18)
  )

  testthat::expect_true(
    all(results$peds$epatient_15 < 18)
  )
})
