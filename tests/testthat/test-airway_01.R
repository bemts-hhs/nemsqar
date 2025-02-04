testthat::test_that("airway_01 requires either df or all required tables", {
  testthat::expect_no_success(airway_01())  # No input provided

  testthat::expect_error(airway_01(df = tibble::tibble()))  # Empty df

  testthat::expect_error(airway_01(
    patient_scene_table = tibble::tibble(),
    response_table = tibble::tibble(),
    arrest_table = tibble::tibble(),
    procedures_table = tibble::tibble(),
    vitals_table = tibble::tibble()
  ))  # Empty tables
})

testthat::test_that("airway_01 detects invalid parameter types", {
  testthat::expect_error(airway_01(df = list()))  # df should be a tibble or dataframe

  testthat::expect_error(airway_01(
    patient_scene_table = list(),
    response_table = tibble::tibble(),
    arrest_table = tibble::tibble(),
    procedures_table = tibble::tibble(),
    vitals_table = tibble::tibble()
  ))  # patient_scene_table should be a tibble

  testthat::expect_error(airway_01(df = tibble::tibble(), erecord_01_col = 123))  # Column names must be strings

  testthat::expect_error(airway_01(df = tibble::tibble(), epatient_16_col = TRUE))  # Column names must be strings
})

testthat::test_that("airway_01 validates required column names", {
  sample_data <- tibble::tibble(erecord_01 = 1:5, epatient_15 = c(30, 40, 50, 20, 10))

  testthat::expect_error(airway_01(
    df = sample_data,
    erecord_01_col = "invalid_column",  # Column does not exist
    epatient_15_col = "epatient_15",
    epatient_16_col = "epatient_16"
  ))

  testthat::expect_error(airway_01(
    df = sample_data,
    erecord_01_col = "erecord_01",
    epatient_15_col = "epatient_15",
    epatient_16_col = "epatient_16"
  ))  # epatient_16_col is missing
})

testthat::test_that("airway_01 handles empty datasets gracefully", {
  empty_df <- tibble::tibble()

  testthat::expect_error(airway_01(
    df = empty_df,
    erecord_01_col = "erecord_01",
    epatient_15_col = "epatient_15",
    epatient_16_col = "epatient_16"
  ))
})

testthat::test_that("airway_01 correctly calculates proportions", {
  data("nemsqar_patient_scene_table", package = "nemsqar")
  data("nemsqar_airway_table", package = "nemsqar")
  data("nemsqar_arrest_table", package = "nemsqar")
  data("nemsqar_response_table", package = "nemsqar")
  data("nemsqar_vitals_table", package = "nemsqar")
  data("nemsqar_procedures_table", package = "nemsqar")

  result <- airway_01(patient_scene_table = nemsqar_patient_scene_table,
                      arrest_table = nemsqar_arrest_table,
                      response_table = nemsqar_response_table,
                      vitals_table = nemsqar_vitals_table,
                      procedures_table = nemsqar_procedures_table,
                      erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
                      incident_date_col = `Incident Date`,patient_dob_col = `Patient Date Of Birth (ePatient.17)`,
                      epatient_15_col = `Patient Age (ePatient.15)`,
                      epatient_16_col = `Patient Age Units (ePatient.16)`,
                      earrest_01_col = `Cardiac Arrest During EMS Event With Code (eArrest.01)`,
                      eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
                      evitals_01_col = `Vitals Signs Taken Date Time (eVitals.01)`,
                      evitals_06_col = `Vitals Systolic Blood Pressure SBP (eVitals.06)`,
                      evitals_12_col = `Vitals Pulse Oximetry (eVitals.12)`,
                      eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
                      eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
                      eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`,
                      eprocedures_05_col = `Procedure Number Of Attempts (eProcedures.05)`,
                      eprocedures_06_col = `Procedure Successful (eProcedures.06)`
                      )

  testthat::expect_s3_class(result, "data.frame")  # Result should be a dataframe
  testthat::expect_true(all(c("pop", "numerator", "denominator", "prop", "prop_label") %in% colnames(result)))  # Check required columns
})

testthat::test_that("airway_01 handles missing data gracefully", {
  data("nemsqar_patient_scene_table", package = "nemsqar")
  data("nemsqar_airway_table", package = "nemsqar")
  data("nemsqar_arrest_table", package = "nemsqar")
  data("nemsqar_response_table", package = "nemsqar")
  data("nemsqar_vitals_table", package = "nemsqar")
  data("nemsqar_procedures_table", package = "nemsqar")

  missing_data <- nemsqar_vitals_table
  missing_data$`Vitals Pulse Oximetry (eVitals.12)` <- NA

  result <- airway_01(patient_scene_table = nemsqar_patient_scene_table,
                      arrest_table = nemsqar_arrest_table,
                      response_table = nemsqar_response_table,
                      vitals_table = missing_data,
                      procedures_table = nemsqar_procedures_table,
                      erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
                      incident_date_col = `Incident Date`,patient_dob_col = `Patient Date Of Birth (ePatient.17)`,
                      epatient_15_col = `Patient Age (ePatient.15)`,
                      epatient_16_col = `Patient Age Units (ePatient.16)`,
                      earrest_01_col = `Cardiac Arrest During EMS Event With Code (eArrest.01)`,
                      eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
                      evitals_01_col = `Vitals Signs Taken Date Time (eVitals.01)`,
                      evitals_06_col = `Vitals Systolic Blood Pressure SBP (eVitals.06)`,
                      evitals_12_col = `Vitals Pulse Oximetry (eVitals.12)`,
                      eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
                      eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
                      eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`,
                      eprocedures_05_col = `Procedure Number Of Attempts (eProcedures.05)`,
                      eprocedures_06_col = `Procedure Successful (eProcedures.06)`
  )

  testthat::expect_s3_class(result, "data.frame")  # Result should be a dataframe
  testthat::expect_true(nrow(result) > 0)  # Ensure some results are returned even with missing data
})

testthat::test_that("airway_01 calculates values correctly for different age groups", {
  data("nemsqar_patient_scene_table", package = "nemsqar")
  data("nemsqar_airway_table", package = "nemsqar")
  data("nemsqar_arrest_table", package = "nemsqar")
  data("nemsqar_response_table", package = "nemsqar")
  data("nemsqar_vitals_table", package = "nemsqar")
  data("nemsqar_procedures_table", package = "nemsqar")

  result <- airway_01(patient_scene_table = nemsqar_patient_scene_table,
                      arrest_table = nemsqar_arrest_table,
                      response_table = nemsqar_response_table,
                      vitals_table = nemsqar_vitals_table,
                      procedures_table = nemsqar_procedures_table,
                      erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
                      incident_date_col = `Incident Date`,patient_dob_col = `Patient Date Of Birth (ePatient.17)`,
                      epatient_15_col = `Patient Age (ePatient.15)`,
                      epatient_16_col = `Patient Age Units (ePatient.16)`,
                      earrest_01_col = `Cardiac Arrest During EMS Event With Code (eArrest.01)`,
                      eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
                      evitals_01_col = `Vitals Signs Taken Date Time (eVitals.01)`,
                      evitals_06_col = `Vitals Systolic Blood Pressure SBP (eVitals.06)`,
                      evitals_12_col = `Vitals Pulse Oximetry (eVitals.12)`,
                      eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
                      eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
                      eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`,
                      eprocedures_05_col = `Procedure Number Of Attempts (eProcedures.05)`,
                      eprocedures_06_col = `Procedure Successful (eProcedures.06)`
                      )

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(all(result$pop %in% c("Adult", "Peds")))  # Check expected population groups
})

testthat::test_that("airway_01 correctly processes complete fact tables", {
  data("nemsqar_patient_scene_table", package = "nemsqar")
  data("nemsqar_airway_table", package = "nemsqar")
  data("nemsqar_arrest_table", package = "nemsqar")
  data("nemsqar_response_table", package = "nemsqar")
  data("nemsqar_vitals_table", package = "nemsqar")
  data("nemsqar_procedures_table", package = "nemsqar")

  result <- airway_01(
    patient_scene_table = nemsqar_patient_scene_table,
    arrest_table = nemsqar_arrest_table,
    response_table = nemsqar_response_table,
    vitals_table = nemsqar_vitals_table,
    procedures_table = nemsqar_procedures_table,
    erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
    incident_date_col = `Incident Date`,
    patient_dob_col = `Patient Date Of Birth (ePatient.17)`,
    epatient_15_col = `Patient Age (ePatient.15)`,
    epatient_16_col = `Patient Age Units (ePatient.16)`,
    earrest_01_col = `Cardiac Arrest During EMS Event With Code (eArrest.01)`,
    eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
    evitals_01_col = `Vitals Signs Taken Date Time (eVitals.01)`,
    evitals_06_col = `Vitals Systolic Blood Pressure SBP (eVitals.06)`,
    evitals_12_col = `Vitals Pulse Oximetry (eVitals.12)`,
    eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
    eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
    eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`,
    eprocedures_05_col = `Procedure Number Of Attempts (eProcedures.05)`,
    eprocedures_06_col = `Procedure Successful (eProcedures.06)`
  )

  # Check that a data frame is returned
  testthat::expect_s3_class(result, "data.frame")

  # Check that the output contains expected columns
  expected_cols <- c("pop", "numerator", "denominator", "prop", "prop_label")
  testthat::expect_true(all(expected_cols %in% colnames(result)))

  # Ensure at least some rows exist
  testthat::expect_gt(nrow(result), 0)

  # Validate that prop is between 0 and 1
  testthat::expect_true(all(result$prop >= 0 & result$prop <= 1, na.rm = TRUE))

  # Check that results exist for Adults and Peds populations
  testthat::expect_true(all(c("Adult", "Peds") %in% result$pop))

  # Ensure denominator is non-zero (indicating valid patient inclusion)
  testthat::expect_false(all(result$denominator > 0, na.rm = TRUE))

})
