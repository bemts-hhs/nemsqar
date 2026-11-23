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

# tests ----

testthat::test_that("data_02 errors when both df and tables are supplied", {
  testthat::expect_error(
    data_02(
      df = test_data_02,
      patient_scene_table = test_data_02,
      audit_table = test_data_02,
      disposition_table = test_data_02,
      times_table = test_data_02,
      erecord_01_col = erecord_01,
      incident_date_col = incident_date,
      patient_DOB_col = patient_dob,
      epatient_15_col = epatient_15,
      epatient_16_col = epatient_16,
      patient_care_col = patient_disposition,
      time_available_to_document_col = unit_back_in_service,
      time_documentation_occurred_col = incident_created
    )
  )
})

testthat::test_that("data_02 errors when no data source supplied", {
  testthat::expect_error(
    data_02(
      erecord_01_col = erecord_01,
      incident_date_col = incident_date,
      patient_DOB_col = patient_dob,
      epatient_15_col = epatient_15,
      epatient_16_col = epatient_16,
      patient_care_col = patient_disposition,
      time_available_to_document_col = unit_back_in_service,
      time_documentation_occurred_col = incident_created
    )
  )
})

testthat::test_that("data_02 returns expected top-level structure", {
  results <- data_02(
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

  testthat::expect_type(results, "list")

  testthat::expect_named(
    results,
    c("distribution", "counts")
  )
})

testthat::test_that("distribution output contains overall results", {
  results <- data_02(
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
    "overall" %in%
      names(results$distribution)
  )
})

testthat::test_that("distribution output includes hours and days", {
  results <- data_02(
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

  testthat::expect_setequal(
    unique(results$distribution$overall$time_unit),
    c("Hours", "Days")
  )
})

testthat::test_that("distribution output contains all populations", {
  results <- data_02(
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

  testthat::expect_setequal(
    unique(results$distribution$overall$pop),
    c("All", "Adults", "Peds")
  )
})

testthat::test_that("count output contains expected columns", {
  results <- data_02(
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
        "documentation_time_band",
        "numerator",
        "denominator",
        "prop"
      ) %in%
        names(results$counts$overall)
    )
  )
})

testthat::test_that("confidence intervals can be requested", {
  results <- suppressWarnings(data_02(
    df = test_data_02,
    erecord_01_col = erecord_01,
    incident_date_col = incident_date,
    patient_DOB_col = patient_dob,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    patient_care_col = patient_disposition,
    time_available_to_document_col = unit_back_in_service,
    time_documentation_occurred_col = incident_created,
    confidence_interval = TRUE
  ))

  testthat::expect_warning(
    object = data_02(
      df = test_data_02,
      erecord_01_col = erecord_01,
      incident_date_col = incident_date,
      patient_DOB_col = patient_dob,
      epatient_15_col = epatient_15,
      epatient_16_col = epatient_16,
      patient_care_col = patient_disposition,
      time_available_to_document_col = unit_back_in_service,
      time_documentation_occurred_col = incident_created,
      confidence_interval = TRUE
    )
  )

  testthat::expect_true(
    all(
      c(
        "lower_ci",
        "upper_ci"
      ) %in%
        names(results$counts$overall)
    )
  )
})

testthat::test_that("stratified results are created when group_vars supplied", {
  strat_data <- test_data_02

  strat_data$vendor <- c(
    "A",
    "A",
    "B",
    "B"
  )

  results <- data_02(
    df = strat_data,
    erecord_01_col = erecord_01,
    incident_date_col = incident_date,
    patient_DOB_col = patient_dob,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    patient_care_col = patient_disposition,
    time_available_to_document_col = unit_back_in_service,
    time_documentation_occurred_col = incident_created,
    group_vars = "vendor"
  )

  testthat::expect_true(
    "stratified" %in%
      names(results$distribution)
  )

  testthat::expect_true(
    "stratified" %in%
      names(results$counts)
  )
})

testthat::test_that("table workflow produces results", {
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

  results <- data_02(
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

  testthat::expect_named(
    results,
    c("distribution", "counts")
  )
})
