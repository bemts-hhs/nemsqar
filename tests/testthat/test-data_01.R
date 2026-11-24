# test-data_01.R

# Synthetic fixture ---------------------------------------------------------

test_data_01 <- tibble::tibble(
  erecord_01 = paste0("R", 1:10),
  incident_date = as.Date("2024-01-01"),
  patient_dob = as.Date(c(
    "1980-01-01",
    "1975-01-01",
    "1990-01-01",
    "1985-01-01",
    "1970-01-01",
    "2015-01-01",
    "2012-01-01",
    "2010-01-01",
    "2018-01-01",
    "2016-01-01"
  )),
  epatient_15 = c(
    44,
    49,
    34,
    39,
    54,
    9,
    12,
    14,
    6,
    8
  ),
  epatient_16 = rep("Years", 10),
  patient_disposition = rep(
    "Patient Treated, Transported by this EMS Unit (4212033)",
    10
  ),
  incident_validity_score = c(
    1.00,
    0.98,
    0.96,
    0.92,
    0.89,
    0.84,
    0.77,
    0.65,
    0.54,
    0.41
  ),
  vendor = rep(
    c("Vendor A", "Vendor B"),
    each = 5
  )
)

# Structure -----------------------------------------------------------------

testthat::test_that("data_01 returns expected top-level objects", {
  results <- nemsqar::data_01(
    df = test_data_01,
    erecord_01_col = erecord_01,
    incident_date_col = incident_date,
    patient_DOB_col = patient_dob,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    patient_care_col = patient_disposition,
    validity_score_col = incident_validity_score
  )

  testthat::expect_type(results, "list")

  testthat::expect_named(
    results,
    c(
      "distribution",
      "counts"
    )
  )
})

testthat::test_that("distribution output contains overall results", {
  results <- nemsqar::data_01(
    df = test_data_01,
    erecord_01_col = erecord_01,
    incident_date_col = incident_date,
    patient_DOB_col = patient_dob,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    patient_care_col = patient_disposition,
    validity_score_col = incident_validity_score
  )

  testthat::expect_s3_class(
    results$distribution$overall,
    "data.frame"
  )
})

testthat::test_that("count output contains overall results", {
  results <- nemsqar::data_01(
    df = test_data_01,
    erecord_01_col = erecord_01,
    incident_date_col = incident_date,
    patient_DOB_col = patient_dob,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    patient_care_col = patient_disposition,
    validity_score_col = incident_validity_score
  )

  testthat::expect_s3_class(
    results$counts$overall,
    "data.frame"
  )
})

# Population summaries ------------------------------------------------------

testthat::test_that("all populations are represented", {
  results <- nemsqar::data_01(
    df = test_data_01,
    erecord_01_col = erecord_01,
    incident_date_col = incident_date,
    patient_DOB_col = patient_dob,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    patient_care_col = patient_disposition,
    validity_score_col = incident_validity_score
  )

  testthat::expect_setequal(
    unique(results$distribution$overall$pop),
    c("All", "Adults", "Peds")
  )
})

# Confidence intervals ------------------------------------------------------

testthat::test_that("confidence intervals are added when requested", {
  results <- suppressWarnings(nemsqar::data_01(
    df = test_data_01,
    erecord_01_col = erecord_01,
    incident_date_col = incident_date,
    patient_DOB_col = patient_dob,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    patient_care_col = patient_disposition,
    validity_score_col = incident_validity_score,
    confidence_interval = TRUE
  ))

  testthat::expect_true(
    "lower_ci" %in%
      names(results$counts$overall)
  )

  testthat::expect_true(
    "upper_ci" %in%
      names(results$counts$overall)
  )
})

testthat::expect_warning(
  object = nemsqar::data_01(
    df = test_data_01,
    erecord_01_col = erecord_01,
    incident_date_col = incident_date,
    patient_DOB_col = patient_dob,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    patient_care_col = patient_disposition,
    validity_score_col = incident_validity_score,
    confidence_interval = TRUE
  )
)

# Stratified analysis -------------------------------------------------------

testthat::test_that("stratified outputs are returned when group_vars supplied", {
  results <- nemsqar::data_01(
    df = test_data_01,
    erecord_01_col = erecord_01,
    incident_date_col = incident_date,
    patient_DOB_col = patient_dob,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    patient_care_col = patient_disposition,
    validity_score_col = incident_validity_score,
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

# Table workflow ------------------------------------------------------------

testthat::test_that("table workflow returns output", {
  patient_scene_table <- test_data_01 |>
    dplyr::select(
      -patient_disposition,
      -incident_validity_score,
      -vendor
    )

  audit_table <- test_data_01 |>
    dplyr::select(
      erecord_01,
      incident_validity_score,
      vendor
    )

  disposition_table <- test_data_01 |>
    dplyr::select(
      erecord_01,
      patient_disposition
    )

  results <- nemsqar::data_01(
    patient_scene_table = patient_scene_table,
    audit_table = audit_table,
    disposition_table = disposition_table,
    erecord_01_col = erecord_01,
    incident_date_col = incident_date,
    patient_DOB_col = patient_dob,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    patient_care_col = patient_disposition,
    validity_score_col = incident_validity_score
  )

  testthat::expect_type(results, "list")
})

# Validation ----------------------------------------------------------------

testthat::test_that("errors when both df and table workflow supplied", {
  testthat::expect_error(
    nemsqar::data_01(
      df = test_data_01,
      patient_scene_table = test_data_01,
      erecord_01_col = erecord_01,
      incident_date_col = incident_date,
      patient_DOB_col = patient_dob,
      epatient_15_col = epatient_15,
      epatient_16_col = epatient_16,
      patient_care_col = patient_disposition,
      validity_score_col = incident_validity_score
    )
  )
})

testthat::test_that("errors when no data source supplied", {
  testthat::expect_error(
    nemsqar::data_01(
      erecord_01_col = erecord_01,
      incident_date_col = incident_date,
      patient_DOB_col = patient_dob,
      epatient_15_col = epatient_15,
      epatient_16_col = epatient_16,
      patient_care_col = patient_disposition,
      validity_score_col = incident_validity_score
    )
  )
})
