# test-data_01_population.R

# Shared synthetic fixture --------------------------------------------------

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
  )
)

# Basic functionality -------------------------------------------------------

testthat::test_that("data_01_population returns a named list", {
  result <- nemsqar::data_01_population(
    df = test_data_01,
    erecord_01_col = erecord_01,
    incident_date_col = incident_date,
    patient_DOB_col = patient_dob,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    patient_care_col = patient_disposition,
    validity_score_col = incident_validity_score
  )

  testthat::expect_type(result, "list")

  testthat::expect_named(
    result,
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

testthat::test_that("adult and pediatric populations are created", {
  result <- nemsqar::data_01_population(
    df = test_data_01,
    erecord_01_col = erecord_01,
    incident_date_col = incident_date,
    patient_DOB_col = patient_dob,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    patient_care_col = patient_disposition,
    validity_score_col = incident_validity_score
  )

  testthat::expect_gt(nrow(result$adults), 0)
  testthat::expect_gt(nrow(result$peds), 0)

  testthat::expect_equal(
    nrow(result$adults),
    5
  )

  testthat::expect_equal(
    nrow(result$peds),
    5
  )
})

testthat::test_that("initial population contains all treated patients", {
  result <- nemsqar::data_01_population(
    df = test_data_01,
    erecord_01_col = erecord_01,
    incident_date_col = incident_date,
    patient_DOB_col = patient_dob,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    patient_care_col = patient_disposition,
    validity_score_col = incident_validity_score
  )

  testthat::expect_equal(
    nrow(result$initial_population),
    10
  )
})

# Derived variables ---------------------------------------------------------

testthat::test_that("validity score variables are created", {
  result <- nemsqar::data_01_population(
    df = test_data_01,
    erecord_01_col = erecord_01,
    incident_date_col = incident_date,
    patient_DOB_col = patient_dob,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    patient_care_col = patient_disposition,
    validity_score_col = incident_validity_score
  )

  required_vars <- c(
    "PATIENT_CARE",
    "VALIDITY_SCORE_FORMAT",
    "VALIDITY_SCORE_RANGE",
    "VALIDITY_SCORE_RANGE_FORMAT"
  )

  testthat::expect_true(
    all(
      required_vars %in%
        names(result$computing_population)
    )
  )
})

testthat::test_that("patient care indicator is created", {
  result <- nemsqar::data_01_population(
    df = test_data_01,
    erecord_01_col = erecord_01,
    incident_date_col = incident_date,
    patient_DOB_col = patient_dob,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    patient_care_col = patient_disposition,
    validity_score_col = incident_validity_score
  )

  testthat::expect_true(
    all(result$computing_population$PATIENT_CARE)
  )
})

# Table workflow ------------------------------------------------------------

testthat::test_that("table workflow returns expected output", {
  patient_scene_table <- test_data_01 |>
    dplyr::select(
      -patient_disposition,
      -incident_validity_score
    )

  audit_table <- test_data_01 |>
    dplyr::select(
      erecord_01,
      incident_validity_score
    )

  disposition_table <- test_data_01 |>
    dplyr::select(
      erecord_01,
      patient_disposition
    )

  result <- nemsqar::data_01_population(
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

  testthat::expect_type(result, "list")

  testthat::expect_equal(
    nrow(result$initial_population),
    10
  )
})

# Validation ----------------------------------------------------------------

testthat::test_that("errors when both df and tables are supplied", {
  patient_scene_table <- test_data_01 |>
    dplyr::select(
      -patient_disposition,
      -incident_validity_score
    )

  audit_table <- test_data_01 |>
    dplyr::select(
      erecord_01,
      incident_validity_score
    )

  disposition_table <- test_data_01 |>
    dplyr::select(
      erecord_01,
      patient_disposition
    )

  testthat::expect_error(
    nemsqar::data_01_population(
      df = test_data_01,
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
  )
})

testthat::test_that("errors when no data source is supplied", {
  testthat::expect_error(
    nemsqar::data_01_population(
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

testthat::test_that("errors when validity scores are outside [0,1]", {
  invalid_data <- test_data_01

  invalid_data$incident_validity_score[1] <- 1.5

  testthat::expect_error(
    nemsqar::data_01_population(
      df = invalid_data,
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

# Age classification branch without date variables ---------------------------

testthat::test_that("data_01_population classifies adults and peds using reported age only", {
  result <- nemsqar::data_01_population(
    df = test_data_01,
    erecord_01_col = erecord_01,
    incident_date_col = NULL,
    patient_DOB_col = NULL,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    patient_care_col = patient_disposition,
    validity_score_col = incident_validity_score
  )

  # adult ages in fixture: 44, 49, 34, 39, 54
  testthat::expect_equal(
    nrow(result$adults),
    5
  )

  # pediatric ages in fixture: 9, 12, 14, 6, 8
  testthat::expect_equal(
    nrow(result$peds),
    5
  )
})

testthat::test_that("data_01_population creates age classification variables without dates", {
  result <- nemsqar::data_01_population(
    df = test_data_01,
    erecord_01_col = erecord_01,
    incident_date_col = NULL,
    patient_DOB_col = NULL,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    patient_care_col = patient_disposition,
    validity_score_col = incident_validity_score
  )

  testthat::expect_true(
    "system_age_adult" %in%
      names(result$computing_population)
  )

  testthat::expect_true(
    "system_age_minor" %in%
      names(result$computing_population)
  )

  # calculated age variables should not exist when dates are omitted
  testthat::expect_false(
    "calc_age_adult" %in%
      names(result$computing_population)
  )

  testthat::expect_false(
    "calc_age_minor" %in%
      names(result$computing_population)
  )
})
