test_that("Test for missing column names", {
  # Load necessary test data
  data("nemsqar_airway_table")
  data("nemsqar_patient_scene_table")
  data("nemsqar_response_table")
  data("nemsqar_vitals_table")
  data("nemsqar_procedures_table")

  expect_error(airway_18_population(
    df = NULL,
    patient_scene_table = nemsqar_patient_scene_table,
    procedures_table = nemsqar_procedures_table,
    vitals_table = nemsqar_vitals_table,
    airway_table = nemsqar_airway_table,
    response_table = nemsqar_response_table,
    erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`
  ), "arguments is missing")
})

test_that("Test for missing df and tables", {
  expect_error(airway_18_population(
    df = NULL,
    patient_scene_table = NULL,
    procedures_table = NULL,
    vitals_table = NULL,
    airway_table = NULL,
    response_table = NULL,
    erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
    incident_date_col = `Incident Date`,
    patient_dob_col = `Patient Date Of Birth (ePatient.17)`,
    epatient_15_col = `Patient Age (ePatient.15)`,
    epatient_16_col = `Patient Age Units (ePatient.16)`,
    eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
    eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
    eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
    eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`,
    eprocedures_05_col = `Procedure Number Of Attempts (eProcedures.05)`,
    eprocedures_06_col = `Procedure Successful (eProcedures.06)`,
    eairway_02_col = `Airway Device Placement Confirmation Date Time (eAirway.02)`,
    eairway_04_col = `Airway Device Placement Confirmed Method List (eAirway.04)`,
    evitals_01_col = `Vitals Signs Taken Date Time (eVitals.01)`,
    evitals_16_col = `Vitals Carbon Dioxide CO2 (eVitals.16)`
  ))
})

test_that("Test for correct input handling (table method)", {
  # Load necessary test data
  data("nemsqar_airway_table")
  data("nemsqar_patient_scene_table")
  data("nemsqar_response_table")
  data("nemsqar_vitals_table")
  data("nemsqar_procedures_table")

  result <- airway_18_population(
    df = NULL,
    patient_scene_table = nemsqar_patient_scene_table,
    procedures_table = nemsqar_procedures_table,
    vitals_table = nemsqar_vitals_table,
    airway_table = nemsqar_airway_table,
    response_table = nemsqar_response_table,
    erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
    incident_date_col = `Incident Date`,
    patient_dob_col = `Patient Date Of Birth (ePatient.17)`,
    epatient_15_col = `Patient Age (ePatient.15)`,
    epatient_16_col = `Patient Age Units (ePatient.16)`,
    eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
    eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
    eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
    eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`,
    eprocedures_05_col = `Procedure Number Of Attempts (eProcedures.05)`,
    eprocedures_06_col = `Procedure Successful (eProcedures.06)`,
    eairway_02_col = `Airway Device Placement Confirmation Date Time (eAirway.02)`,
    eairway_04_col = `Airway Device Placement Confirmed Method List (eAirway.04)`,
    evitals_01_col = `Vitals Signs Taken Date Time (eVitals.01)`,
    evitals_16_col = `Vitals Carbon Dioxide CO2 (eVitals.16)`
  )
  expect_type(result, "list")
  expect_true(all(names(result) %in% c("filter_process", "adults", "peds", "initial_population")))
})

test_that("Test for invalid column names", {
  expect_error(airway_18_population(
    df = nemsqar_airway_18_df,
    patient_scene_table = NULL,
    procedures_table = NULL,
    vitals_table = NULL,
    airway_table = NULL,
    response_table = NULL,
    erecord_01_col = `Invalid Column Name`,
    incident_date_col = `Incident Date`,
    patient_dob_col = `Patient Date Of Birth (ePatient.17)`,
    epatient_15_col = `Patient Age (ePatient.15)`,
    epatient_16_col = `Patient Age Units (ePatient.16)`,
    eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
    eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
    eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
    eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`,
    eprocedures_05_col = `Procedure Number Of Attempts (eProcedures.05)`,
    eprocedures_06_col = `Procedure Successful (eProcedures.06)`,
    eairway_02_col = `Airway Device Placement Confirmation Date Time (eAirway.02)`,
    eairway_04_col = `Airway Device Placement Confirmed Method List (eAirway.04)`,
    evitals_01_col = `Vitals Signs Taken Date Time (eVitals.01)`,
    evitals_16_col = `Vitals Carbon Dioxide CO2 (eVitals.16)`
  ))
})

test_that("Test for table and df input conflicts", {

  df <- tibble::tibble()

  expect_error(airway_18_population(
    df = nemsqar_airway_18_df,
    patient_scene_table = nemsqar_patient_scene_table,
    procedures_table = nemsqar_procedures_table,
    vitals_table = nemsqar_vitals_table,
    airway_table = nemsqar_airway_table,
    response_table = nemsqar_response_table,
    erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
    incident_date_col = `Incident Date`,
    patient_dob_col = `Patient Date Of Birth (ePatient.17)`,
    epatient_15_col = `Patient Age (ePatient.15)`,
    epatient_16_col = `Patient Age Units (ePatient.16)`,
    eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
    eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
    eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
    eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`,
    eprocedures_05_col = `Procedure Number Of Attempts (eProcedures.05)`,
    eprocedures_06_col = `Procedure Successful (eProcedures.06)`,
    eairway_02_col = `Airway Device Placement Confirmation Date Time (eAirway.02)`,
    eairway_04_col = `Airway Device Placement Confirmed Method List (eAirway.04)`,
    evitals_01_col = `Vitals Signs Taken Date Time (eVitals.01)`,
    evitals_16_col = `Vitals Carbon Dioxide CO2 (eVitals.16)`
  ))
})

# Example test: Check if intubation data is correctly filtered
test_that("incident date is correctly validated", {

  # Mock data setup
  patient_scene_table <- tibble::tibble(
    erecord_01_col = 1:5,
    incident_date_col = c(5, 10, 15, 20, 25),
    patient_dob_col = c(20, 25, 30, 35, 40),
    epatient_15 = c(20, 30, 40, 50, 60),
    epatient_16 = c("Years", "Months", "Days", "Years", "Months")
  )

  procedures_table <- tibble::tibble(
    erecord_01_col = 1:5,
    eprocedures_01_col = Sys.time() - c(1, 3, 5, 7, 9),
    eprocedures_02_col = c("Yes", "No", "Yes", "No", "Yes"),
    eprocedures_03_col = c("endotracheal intubation", "other", "endotracheal intubation", "other", "endotracheal intubation"),
    eprocedures_05_col = c(1, 5, 1, 3, 1),
    eprocedures_06_col = c("yes", "no", "yes", "no", "yes")
  )

  response_table <- tibble::tibble(
    erecord_01_col = 1:5,
    eresponse_05_col = c("911", "911", "other", "other", "911")
  )

  vitals_table <- tibble::tibble(
    erecord_01_col = 1:5,
    evitals_01_col = Sys.time() + c(0, 1, 2, 3, 4),
    evitals_16_col = c(6, 4, 8, 7, 9)
  )

  airway_table <- tibble::tibble(
    erecord_01_col = 1:5,
    eairway_02_col = Sys.time() + c(1, 3, 5, 7, 9),
    eairway_04_col = c("waveform etco2", "other", "waveform etco2", "other", "waveform etco2")
  )

  testthat::expect_error(airway_18_population(
    patient_scene_table = patient_scene_table,
    procedures_table = procedures_table,
    vitals_table = vitals_table,
    airway_table = airway_table,
    response_table = response_table,
    erecord_01_col = erecord_01_col,
    incident_date_col = incident_date_col,
    patient_dob_col = patient_dob_col,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05_col,
    eprocedures_01_col = eprocedures_01_col,
    eprocedures_02_col = eprocedures_02_col,
    eprocedures_03_col = eprocedures_03_col,
    eprocedures_05_col = eprocedures_05_col,
    eprocedures_06_col = eprocedures_06_col,
    eairway_02_col = eairway_02_col,
    eairway_04_col = eairway_04_col,
    evitals_01_col = evitals_01_col,
    evitals_16_col = evitals_16_col
  ))

})

# Test case 1: Verify patient age calculation when both dates are provided
test_that("vitals times are correctly validated", {

  # Mock data setup
  patient_scene_table <- tibble::tibble(
    erecord_01_col = 1:5,
    incident_date_col = Sys.time() - c(5, 10, 15, 20, 25),
    patient_dob_col = Sys.time() - c(1120, 1125, 1130, 1135, 1140),
    epatient_15 = c(20, 30, 40, 50, 60),
    epatient_16 = c("Years", "Months", "Days", "Years", "Months")
  )

  procedures_table <- tibble::tibble(
    erecord_01_col = 1:5,
    eprocedures_01_col = Sys.time() - c(1, 3, 5, 7, 9),
    eprocedures_02_col = c("Yes", "No", "Yes", "No", "Yes"),
    eprocedures_03_col = c("endotracheal intubation", "other", "endotracheal intubation", "other", "endotracheal intubation"),
    eprocedures_05_col = c(1, 5, 1, 3, 1),
    eprocedures_06_col = c("yes", "no", "yes", "no", "yes")
  )

  response_table <- tibble::tibble(
    erecord_01_col = 1:5,
    eresponse_05_col = c("911", "911", "other", "other", "911")
  )

  vitals_table <- tibble::tibble(
    erecord_01_col = 1:5,
    evitals_01_col = c(0, 1, 2, 3, 4),
    evitals_16_col = c(6, 4, 8, 7, 9)
  )

  airway_table <- tibble::tibble(
    erecord_01_col = 1:5,
    eairway_02_col = Sys.time() + c(1, 3, 5, 7, 9),
    eairway_04_col = c("waveform etco2", "other", "waveform etco2", "other", "waveform etco2")
  )

  testthat::expect_error(airway_18_population(
    patient_scene_table = patient_scene_table,
    procedures_table = procedures_table,
    vitals_table = vitals_table,
    airway_table = airway_table,
    response_table = response_table,
    erecord_01_col = erecord_01_col,
    incident_date_col = incident_date_col,
    patient_dob_col = patient_dob_col,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05_col,
    eprocedures_01_col = eprocedures_01_col,
    eprocedures_02_col = eprocedures_02_col,
    eprocedures_03_col = eprocedures_03_col,
    eprocedures_05_col = eprocedures_05_col,
    eprocedures_06_col = eprocedures_06_col,
    eairway_02_col = eairway_02_col,
    eairway_04_col = eairway_04_col,
    evitals_01_col = evitals_01_col,
    evitals_16_col = evitals_16_col
  ))


})

# Test case 2: Check if the numerator data extraction works
test_that("denominator data is correctly extracted", {

  # Mock data setup
  patient_scene_table <- tibble::tibble(
    erecord_01_col = 1:5,
    incident_date_col = Sys.time() + c(5, 10, 15, 20, 25),
    patient_dob_col = Sys.time() - c(1120, 1125, 1130, 1135, 1140),
    epatient_15 = c(20, 30, 40, 50, 60),
    epatient_16 = c("Years", "Months", "Days", "Years", "Months")
  )

  procedures_table <- tibble::tibble(
    erecord_01_col = 1:5,
    eprocedures_01_col = Sys.time() - c(1, 3, 5, 7, 9),
    eprocedures_02_col = c("Yes", "No", "Yes", "No", "Yes"),
    eprocedures_03_col = c("endotracheal intubation", "other", "endotracheal intubation", "other", "endotracheal intubation"),
    eprocedures_05_col = c(1, 5, 1, 3, 1),
    eprocedures_06_col = c("yes", "no", "yes", "no", "yes")
  )

  response_table <- tibble::tibble(
    erecord_01_col = 1:5,
    eresponse_05_col = c("911", "911", "other", "other", "911")
  )

  vitals_table <- tibble::tibble(
    erecord_01_col = 1:5,
    evitals_01_col = Sys.time() + c(0, 1, 2, 3, 4),
    evitals_16_col = c(6, 4, 8, 7, 9)
  )

  airway_table <- tibble::tibble(
    erecord_01_col = 1:5,
    eairway_02_col = Sys.time() + c(1, 3, 5, 7, 9),
    eairway_04_col = c("waveform etco2", "other", "waveform etco2", "other", "waveform etco2")
  )

  result <- airway_18_population(
    patient_scene_table = patient_scene_table,
    procedures_table = procedures_table,
    vitals_table = vitals_table,
    airway_table = airway_table,
    response_table = response_table,
    erecord_01_col = erecord_01_col,
    incident_date_col = incident_date_col,
    patient_dob_col = patient_dob_col,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05_col,
    eprocedures_01_col = eprocedures_01_col,
    eprocedures_02_col = eprocedures_02_col,
    eprocedures_03_col = eprocedures_03_col,
    eprocedures_05_col = eprocedures_05_col,
    eprocedures_06_col = eprocedures_06_col,
    eairway_02_col = eairway_02_col,
    eairway_04_col = eairway_04_col,
    evitals_01_col = evitals_01_col,
    evitals_16_col = evitals_16_col
  )

  filters <- result$filter_process

  testthat::expect_equal(nrow(filters), 12)

})
