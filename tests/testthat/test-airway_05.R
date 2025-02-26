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
                      incident_date_col = `Incident Date`,
                      patient_DOB_col = `Patient Date Of Birth (ePatient.17)`,
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
                      incident_date_col = `Incident Date`,
                      patient_DOB_col = `Patient Date Of Birth (ePatient.17)`,
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

# Test that the function works with a dataframe
testthat::test_that("airway_05 handles a dataframe input", {

    # data
    data <- tibble::tibble(

      erecord_01 = rep(c("R1", "R2", "R3", "R4", "R5"), 2),
      incident_date = rep(as.Date(c("2025-01-01", "2025-01-05", "2025-02-01",
      "2025-01-01", "2025-06-01")), 2),
      patient_dob = rep(as.Date(c("2000-01-01", "2020-01-01", "2023-02-01",
                                  "2023-01-01", "1970-06-01")), 2),
      epatient_15 = rep(c(25, 5, 2, 2, 55), 2),  # Ages
      epatient_16 = rep(c("Years", "Years", "Years", "Years", "Years"), 2),
      eresponse_05 = rep(2205001, 10),
      evitals_01 = lubridate::as_datetime(c("2025-01-01 22:59:00",
      "2025-01-05 11:58:00", "2025-02-01 18:57:00", "2025-01-01 04:58:00",
      "2025-06-01 12:57:00", "2025-01-01 23:05:00", "2025-01-05 12:04:00",
      "2025-02-01 19:03:00", "2025-01-01 05:02:00", "2025-06-01 13:01:00")),
      evitals_12 = rep(c(94, 95, 96, 97, 98), 2),
      earrest_01 = rep("No", 10),
      eprocedures_01 = rep(lubridate::as_datetime(c("2025-01-01 23:00:00",
      "2025-01-05 12:00:00", "2025-02-01 19:00:00", "2025-01-01 05:00:00",
      "2025-06-01 13:00:00")), 2),
      eprocedures_02 = rep("No", 10),
      eprocedures_03 = rep(c(16883004, 112798008, 78121007, 49077009,
                             673005), 2),

    )

  # Run the function
  result <- airway_05(df = data,
           erecord_01_col = erecord_01,
           incident_date_col = incident_date,
           patient_DOB_col = patient_dob,
           epatient_15_col = epatient_15,
           epatient_16_col = epatient_16,
           eresponse_05_col = eresponse_05,
           eprocedures_01_col = eprocedures_01,
           eprocedures_02_col = eprocedures_02,
           eprocedures_03_col = eprocedures_03,
           earrest_01_col = earrest_01,
           evitals_01_col = evitals_01,
           evitals_12_col = evitals_12
           )


  expect_s3_class(result, "data.frame")
  expect_true(all(c("pop", "numerator", "denominator", "prop", "prop_label") %in% names(result)))

})
