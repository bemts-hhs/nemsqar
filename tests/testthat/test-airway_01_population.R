# Function should correctly classify airway cases
testthat::test_that("airway_01_population correctly identifies intubation cases", {
  # Sample synthetic test data
  data("nemsqar_airway_01_df", package = "nemsqar")

  testthat::expect_error(airway_01_population(nemsqar_airway_01_df), "arguments is missing")

  result <- nemsqar_airway_01_df |>
    airway_01_population(erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
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

  testthat::expect_type(result, "list")
  testthat::expect_true(all(c("peds", "initial_population", "adults", "filter_process") %in% names(result)))
  testthat::expect_equal(nrow(result$filter_process), 19)  # Two cases should be TRUE
})

# Function should handle missing values without errors
testthat::test_that("airway_01_population handles missing values gracefully", {
  # Sample synthetic test data
  data("nemsqar_airway_01_df", package = "nemsqar")

  missing_data <- nemsqar_airway_01_df
  missing_data$`Procedure Performed Description And Code (eProcedures.03)` <- NA  # Set all to NA
  result <- airway_01_population(missing_data,
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
  testthat::expect_type(result, "list")
  testthat::expect_true(nrow(result$initial_population) == 0)
})

# Function should return expected types and structure
testthat::test_that("airway_01_population returns expected structure", {
  # Sample synthetic test data
  data("nemsqar_airway_01_df", package = "nemsqar")

  result <- airway_01_population(nemsqar_airway_01_df,
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

  testthat::expect_true(!is.logical(result$initial_population))
  testthat::expect_true(!is.logical(result$adults))
  testthat::expect_true(!is.logical(result$peds))
  testthat::expect_true(!is.logical(result$filter_process))
  testthat::expect_equal(result$filter_process |>
                           dplyr::filter(filter == "Total procedures in dataset") |>
                           dplyr::pull(count),
                         nrow(nemsqar_airway_01_df)
                         )
})

# Function should correctly handle unexpected procedure codes
testthat::test_that("airway_01_population filters unexpected codes", {
  # Sample synthetic test data
  data("nemsqar_airway_01_df", package = "nemsqar")

  unexpected_data <- nemsqar_airway_01_df
  unexpected_data$`Vitals Signs Taken Date Time (eVitals.01)` <- 9999  # Invalid values
  testthat::expect_error(airway_01_population(unexpected_data,
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
                         )

})

# Function should handle empty data without error
testthat::test_that("airway_01_population handles empty input", {
  # Sample synthetic test data
  valid_data <- tibble::tibble(
    eProcedures_03 = c("4250", "1025", NA, "4250"),
    eProcedures_02 = c("201", "202", "203", "201"),
    eVitals_02 = c(120, 60, 90, NA),
    eVitals_03 = c(80, 40, 70, NA),
    ePatient_15 = c(25, 40, 16, 60),
    patient_age_in_months = c(NA, NA, 180, NA)
  )

  empty_data <- valid_data[0, ]  # No rows

  testthat::expect_error(airway_01_population(empty_data), "arguments is missing")

})

# Function should handle non-numeric vitals
testthat::test_that("airway_01_population handles non-numeric vital signs gracefully", {
  # Sample synthetic test data
  data("nemsqar_airway_01_df", package = "nemsqar")

  non_numeric_data <- nemsqar_airway_01_df
  non_numeric_data$`Vitals Signs Taken Date Time (eVitals.01)` <- "high"
  non_numeric_data$`Incident Date` <- "low"
  testthat::expect_error(airway_01_population(non_numeric_data,
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
                                              ))
})
