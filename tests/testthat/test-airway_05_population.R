# Function should correctly classify airway cases
testthat::test_that("airway_05_population correctly identifies intubation cases", {
  # Sample synthetic test data
  data("nemsqar_patient_scene_table", package = "nemsqar")
  data("nemsqar_arrest_table", package = "nemsqar")
  data("nemsqar_response_table", package = "nemsqar")
  data("nemsqar_vitals_table", package = "nemsqar")
  data("nemsqar_procedures_table", package = "nemsqar")

  testthat::expect_error(airway_05_population(patient_scene_table = nemsqar_patient_scene_table), "arguments is missing")

  result <- airway_05_population(
    patient_scene_table = nemsqar_patient_scene_table,
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
    evitals_12_col = `Vitals Pulse Oximetry (eVitals.12)`,
    eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
    eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
    eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`,
  )

  testthat::expect_type(result, "list")
  testthat::expect_true(all(c("peds", "initial_population", "adults", "filter_process") %in% names(result)))
  testthat::expect_equal(nrow(result$filter_process), 11)  # Two cases should be TRUE
})

# Function should handle missing values without errors
testthat::test_that("airway_05_population handles missing values gracefully", {
  # Sample synthetic test data
  data("nemsqar_patient_scene_table", package = "nemsqar")
  data("nemsqar_arrest_table", package = "nemsqar")
  data("nemsqar_response_table", package = "nemsqar")
  data("nemsqar_vitals_table", package = "nemsqar")
  data("nemsqar_procedures_table", package = "nemsqar")

  missing_data <- nemsqar_procedures_table
  missing_data$`Procedure Performed Description And Code (eProcedures.03)` <- NA  # Set all to NA
  result <- airway_05_population(patient_scene_table = nemsqar_patient_scene_table,
                                 arrest_table = nemsqar_arrest_table,
                                 response_table = nemsqar_response_table,
                                 vitals_table = nemsqar_vitals_table,
                                 procedures_table = missing_data,
                                 erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
                                 incident_date_col = `Incident Date`,patient_dob_col = `Patient Date Of Birth (ePatient.17)`,
                                 epatient_15_col = `Patient Age (ePatient.15)`,
                                 epatient_16_col = `Patient Age Units (ePatient.16)`,
                                 earrest_01_col = `Cardiac Arrest During EMS Event With Code (eArrest.01)`,
                                 eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
                                 evitals_01_col = `Vitals Signs Taken Date Time (eVitals.01)`,
                                 evitals_12_col = `Vitals Pulse Oximetry (eVitals.12)`,
                                 eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
                                 eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
                                 eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`,
  )

  testthat::expect_type(result, "list")
  testthat::expect_true(nrow(result$initial_population) == 0)

})

# Function should return expected types and structure
testthat::test_that("airway_05_population returns expected structure", {
  # Sample synthetic test data
  data("nemsqar_patient_scene_table", package = "nemsqar")
  data("nemsqar_arrest_table", package = "nemsqar")
  data("nemsqar_response_table", package = "nemsqar")
  data("nemsqar_vitals_table", package = "nemsqar")
  data("nemsqar_procedures_table", package = "nemsqar")

  result <- airway_05_population(patient_scene_table = nemsqar_patient_scene_table,
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
                                 evitals_12_col = `Vitals Pulse Oximetry (eVitals.12)`,
                                 eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
                                 eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
                                 eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`,
  )

  testthat::expect_true(!is.logical(result$initial_population))
  testthat::expect_true(!is.logical(result$adults))
  testthat::expect_true(!is.logical(result$peds))
  testthat::expect_true(!is.logical(result$filter_process))
  testthat::expect_equal(result$filter_process |>
                           dplyr::filter(filter == "Total procedures in dataset") |>
                           dplyr::pull(count),
                         nrow(nemsqar_patient_scene_table)
  )
})

# Function should correctly handle unexpected procedure codes
testthat::test_that("airway_05_population filters unexpected codes", {
  # Sample synthetic test data
  data("nemsqar_patient_scene_table", package = "nemsqar")
  data("nemsqar_arrest_table", package = "nemsqar")
  data("nemsqar_response_table", package = "nemsqar")
  data("nemsqar_vitals_table", package = "nemsqar")
  data("nemsqar_procedures_table", package = "nemsqar")

  unexpected_data <- nemsqar_vitals_table
  unexpected_data$`Vitals Signs Taken Date Time (eVitals.01)` <- 9999  # Invalid values
  testthat::expect_error(airway_05_population(patient_scene_table = nemsqar_patient_scene_table,
                                              arrest_table = nemsqar_arrest_table,
                                              response_table = nemsqar_response_table,
                                              vitals_table = unexpected_data,
                                              procedures_table = nemsqar_procedures_table,
                                              erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
                                              incident_date_col = `Incident Date`,patient_dob_col = `Patient Date Of Birth (ePatient.17)`,
                                              epatient_15_col = `Patient Age (ePatient.15)`,
                                              epatient_16_col = `Patient Age Units (ePatient.16)`,
                                              earrest_01_col = `Cardiac Arrest During EMS Event With Code (eArrest.01)`,
                                              eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
                                              evitals_01_col = `Vitals Signs Taken Date Time (eVitals.01)`,
                                              evitals_12_col = `Vitals Pulse Oximetry (eVitals.12)`,
                                              eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
                                              eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
                                              eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`,
  )
  )

})

# Function should handle empty data without error
testthat::test_that("airway_05_population handles empty input", {

  valid_data <- tibble::tibble()

  empty_data <- valid_data[0, ]  # No rows

  testthat::expect_error(airway_05_population(empty_data), "arguments is missing")

})

# Function should handle non-numeric vitals
testthat::test_that("airway_05_population handles non-numeric vital signs gracefully", {
  # Sample synthetic test data
  data("nemsqar_patient_scene_table", package = "nemsqar")
  data("nemsqar_arrest_table", package = "nemsqar")
  data("nemsqar_response_table", package = "nemsqar")
  data("nemsqar_vitals_table", package = "nemsqar")
  data("nemsqar_procedures_table", package = "nemsqar")

  non_numeric_data1 <- nemsqar_vitals_table
  non_numeric_data2 <- nemsqar_patient_scene_table
  non_numeric_data1$`Vitals Signs Taken Date Time (eVitals.01)` <- "high"
  non_numeric_data2$`Incident Date` <- "low"
  testthat::expect_error(airway_05_population(patient_scene_table = non_numeric_data2,
                                              arrest_table = nemsqar_arrest_table,
                                              response_table = nemsqar_response_table,
                                              vitals_table = non_numeric_data1,
                                              procedures_table = nemsqar_procedures_table,
                                              erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
                                              incident_date_col = `Incident Date`,patient_dob_col = `Patient Date Of Birth (ePatient.17)`,
                                              epatient_15_col = `Patient Age (ePatient.15)`,
                                              epatient_16_col = `Patient Age Units (ePatient.16)`,
                                              earrest_01_col = `Cardiac Arrest During EMS Event With Code (eArrest.01)`,
                                              eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
                                              evitals_01_col = `Vitals Signs Taken Date Time (eVitals.01)`,
                                              evitals_12_col = `Vitals Pulse Oximetry (eVitals.12)`,
                                              eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
                                              eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
                                              eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`,
  ))
})
