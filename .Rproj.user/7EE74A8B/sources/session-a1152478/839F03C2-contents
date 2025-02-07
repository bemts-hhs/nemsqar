test_that("airway_05 handles valid input correctly", {
  data("nemsqar_patient_scene_table", package = "nemsqar")
  data("nemsqar_airway_table", package = "nemsqar")
  data("nemsqar_arrest_table", package = "nemsqar")
  data("nemsqar_response_table", package = "nemsqar")
  data("nemsqar_vitals_table", package = "nemsqar")
  data("nemsqar_procedures_table", package = "nemsqar")

  result <- airway_05(patient_scene_table = nemsqar_patient_scene_table,
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
                      eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`
                      )

  expect_s3_class(result, "data.frame")
  expect_true(all(c("pop", "numerator", "denominator", "prop", "prop_label") %in% names(result)))
})

# Test missing values handling
test_that("airway_05 handles missing values", {
  data("nemsqar_patient_scene_table", package = "nemsqar")
  data("nemsqar_airway_table", package = "nemsqar")
  data("nemsqar_arrest_table", package = "nemsqar")
  data("nemsqar_response_table", package = "nemsqar")
  data("nemsqar_vitals_table", package = "nemsqar")
  data("nemsqar_procedures_table", package = "nemsqar")

  missing_data <- nemsqar_vitals_table
  missing_data$`Vitals Signs Taken Date Time (eVitals.01)` <- NA

  expect_error(airway_05(patient_scene_table = nemsqar_patient_scene_table,
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
                      evitals_12_col = `Vitals Pulse Oximetry (eVitals.12)`,
                      eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
                      eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
                      eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`
                      )
               )


})

# Test invalid data types
test_that("airway_05 handles invalid data types", {
  df <- tibble::tibble(
    erecord_01 = 1:5,
    epatient_15 = c("twenty", "forty", "sixty", "five", "ten"),
    epatient_16 = c("Years", "Years", "Years", "Years", "Years"),
    earrest_01 = c("No", "No", "Yes", "No", "Yes"),
    eresponse_05 = c("911", "911", "911", "911", "911"),
    evitals_01 = Sys.time() + 1:5,
    evitals_12 = c(98, 92, 88, 90, 95),
    eprocedures_01 = Sys.time() + 1:5,
    eprocedures_02 = c("No", "No", "Yes", "No", "No"),
    eprocedures_03 = c("Intubation", "Intubation", "Intubation", "Intubation", "Intubation")
  )

  expect_error(
    airway_05(df = df,
              erecord_01_col = erecord_01,
              epatient_15_col = epatient_15,
              epatient_16_col = epatient_16,
              earrest_01_col = earrest_01,
              eresponse_05_col = eresponse_05,
              evitals_01_col = evitals_01,
              evitals_12_col = evitals_12,
              eprocedures_01_col = eprocedures_01,
              eprocedures_02_col = eprocedures_02,
              eprocedures_03_col = eprocedures_03)
    )
})
