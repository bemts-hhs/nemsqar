# all NULL arguments to tables and df
# expect an error
testthat::test_that("airway_01_population correctly identifies data entry errors", {


  testthat::expect_error(airway_01_population(
    df = list(),
    erecord_01_col = character(),
    incident_date_col = date(),
    patient_DOB_col = date(),
    epatient_15_col = numeric(),
    epatient_16_col = character(),
    earrest_01_col = character(),
    eresponse_05_col = character(),
    evitals_01_col = date(),
    evitals_06_col = numeric(),
    evitals_12_col = numeric(),
    eprocedures_01_col = date(),
    eprocedures_02_col = character(),
    eprocedures_03_col = character(),
    eprocedures_05_col = character(),
    eprocedures_06_col = character()
  ))

  testthat::expect_error(airway_01_population(
    df = tibble::tibble(),
    patient_scene_table = tibble::tibble(),
    arrest_table = tibble::tibble(),
    response_table = tibble::tibble(),
    vitals_table = tibble::tibble(),
    procedures_table = tibble::tibble(),
    erecord_01_col = character(),
    incident_date_col = date(),
    patient_DOB_col = date(),
    epatient_15_col = numeric(),
    epatient_16_col = character(),
    earrest_01_col = character(),
    eresponse_05_col = character(),
    evitals_01_col = date(),
    evitals_06_col = numeric(),
    evitals_12_col = numeric(),
    eprocedures_01_col = date(),
    eprocedures_02_col = character(),
    eprocedures_03_col = character(),
    eprocedures_05_col = character(),
    eprocedures_06_col = character()
  ))

  testthat::expect_error(airway_01_population(
    df = NULL,
    patient_scene_table = list(),
    arrest_table = tibble::tibble(),
    response_table = tibble::tibble(),
    vitals_table = tibble::tibble(),
    procedures_table = tibble::tibble(),
    erecord_01_col = character(),
    incident_date_col = date(),
    patient_DOB_col = date(),
    epatient_15_col = numeric(),
    epatient_16_col = character(),
    earrest_01_col = character(),
    eresponse_05_col = character(),
    evitals_01_col = date(),
    evitals_06_col = numeric(),
    evitals_12_col = numeric(),
    eprocedures_01_col = date(),
    eprocedures_02_col = character(),
    eprocedures_03_col = character(),
    eprocedures_05_col = character(),
    eprocedures_06_col = character()
  ))

  testthat::expect_error(airway_01_population(
    erecord_01_col = character(),
    incident_date_col = date(),
    patient_DOB_col = date(),
    epatient_15_col = numeric(),
    epatient_16_col = character(),
    earrest_01_col = character(),
    eresponse_05_col = character(),
    evitals_01_col = date(),
    evitals_06_col = numeric(),
    evitals_12_col = numeric(),
    eprocedures_01_col = date(),
    eprocedures_02_col = character(),
    eprocedures_03_col = character(),
    eprocedures_05_col = character(),
    eprocedures_06_col = character()
  ))

  testthat::expect_error(airway_01_population(
    df = NULL,
    patient_scene_table = tibble::tibble(),
    arrest_table = tibble::tibble(),
    response_table = tibble::tibble(),
    vitals_table = tibble::tibble(),
    procedures_table = tibble::tibble(),
    erecord_01_col = character(),
    incident_date_col = character(),
    patient_DOB_col = character(),
    epatient_15_col = numeric(),
    epatient_16_col = character(),
    earrest_01_col = character(),
    eresponse_05_col = character(),
    evitals_01_col = date(),
    evitals_06_col = numeric(),
    evitals_12_col = numeric(),
    eprocedures_01_col = date(),
    eprocedures_02_col = character(),
    eprocedures_03_col = character(),
    eprocedures_05_col = character(),
    eprocedures_06_col = character()
  ))

  testthat::expect_error(airway_01_population(
    df = NULL,
    patient_scene_table = tibble::tibble(),
    arrest_table = tibble::tibble(),
    response_table = tibble::tibble(),
    vitals_table = tibble::tibble(),
    procedures_table = tibble::tibble(),
    erecord_01_col = character(),
    incident_date_col = date(),
    patient_DOB_col = date(),
    epatient_15_col = numeric(),
    epatient_16_col = character(),
    earrest_01_col = character(),
    eresponse_05_col = character(),
    evitals_01_col = character(),
    evitals_06_col = numeric(),
    evitals_12_col = numeric(),
    eprocedures_01_col = character(),
    eprocedures_02_col = character(),
    eprocedures_03_col = character(),
    eprocedures_05_col = character(),
    eprocedures_06_col = character()
  ))

})

# Function should correctly classify airway cases
testthat::test_that("airway_01_population correctly identifies intubation cases", {
  # Sample synthetic test data
  data("nemsqar_patient_scene_table", package = "nemsqar")
  data("nemsqar_arrest_table", package = "nemsqar")
  data("nemsqar_response_table", package = "nemsqar")
  data("nemsqar_vitals_table", package = "nemsqar")
  data("nemsqar_procedures_table", package = "nemsqar")

  testthat::expect_error(airway_01_population(patient_scene_table = nemsqar_patient_scene_table), "arguments is missing")

  result <- airway_01_population(
                         patient_scene_table = nemsqar_patient_scene_table,
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
  data("nemsqar_patient_scene_table", package = "nemsqar")
  data("nemsqar_arrest_table", package = "nemsqar")
  data("nemsqar_response_table", package = "nemsqar")
  data("nemsqar_vitals_table", package = "nemsqar")
  data("nemsqar_procedures_table", package = "nemsqar")

  missing_data <- nemsqar_procedures_table
  missing_data$`Procedure Performed Description And Code (eProcedures.03)` <- NA  # Set all to NA
  expect_warning(airway_01_population(patient_scene_table = nemsqar_patient_scene_table,
                                 arrest_table = nemsqar_arrest_table,
                                 response_table = nemsqar_response_table,
                                 vitals_table = nemsqar_vitals_table,
                                 procedures_table = missing_data,
                                 erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
                                 incident_date_col = `Incident Date`,
                                 patient_DOB_col = `Patient Date Of Birth (ePatient.17)`,
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

# Function should return expected types and structure
testthat::test_that("airway_01_population returns expected structure", {
  # Sample synthetic test data
  data("nemsqar_patient_scene_table", package = "nemsqar")
  data("nemsqar_arrest_table", package = "nemsqar")
  data("nemsqar_response_table", package = "nemsqar")
  data("nemsqar_vitals_table", package = "nemsqar")
  data("nemsqar_procedures_table", package = "nemsqar")

  result <- airway_01_population(patient_scene_table = nemsqar_patient_scene_table,
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
                         nrow(nemsqar_patient_scene_table)
                         )

  result2 <- airway_01_population(patient_scene_table = nemsqar_patient_scene_table,
                                 arrest_table = nemsqar_arrest_table,
                                 response_table = nemsqar_response_table,
                                 vitals_table = nemsqar_vitals_table,
                                 procedures_table = nemsqar_procedures_table,
                                 erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
                                 incident_date_col = NULL,
                                 patient_DOB_col = NULL,
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

  testthat::expect_true(!is.logical(result2$initial_population))
  testthat::expect_true(!is.logical(result2$adults))
  testthat::expect_true(!is.logical(result2$peds))
  testthat::expect_true(!is.logical(result2$filter_process))
  testthat::expect_equal(result2$filter_process |>
                           dplyr::filter(filter == "Total procedures in dataset") |>
                           dplyr::pull(count),
                         nrow(nemsqar_patient_scene_table)
  )

})

# Function should correctly handle unexpected procedure codes
testthat::test_that("airway_01_population filters unexpected codes", {
  # Sample synthetic test data
  data("nemsqar_patient_scene_table", package = "nemsqar")
  data("nemsqar_arrest_table", package = "nemsqar")
  data("nemsqar_response_table", package = "nemsqar")
  data("nemsqar_vitals_table", package = "nemsqar")
  data("nemsqar_procedures_table", package = "nemsqar")

  unexpected_data <- nemsqar_vitals_table
  unexpected_data$`Vitals Signs Taken Date Time (eVitals.01)` <- 9999  # Invalid values
  testthat::expect_error(airway_01_population(patient_scene_table = nemsqar_patient_scene_table,
                                              arrest_table = nemsqar_arrest_table,
                                              response_table = nemsqar_response_table,
                                              vitals_table = unexpected_data,
                                              procedures_table = nemsqar_procedures_table,
                                              erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
                                              incident_date_col = `Incident Date`,
                                              patient_DOB_col = `Patient Date Of Birth (ePatient.17)`,
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

  valid_data <- tibble::tibble()

  empty_data <- valid_data[0, ]  # No rows

  testthat::expect_error(airway_01_population(empty_data), "arguments is missing")

})

# Function should handle non-numeric vitals
testthat::test_that("airway_01_population handles non-numeric vital signs gracefully", {
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
  testthat::expect_error(airway_01_population(patient_scene_table = non_numeric_data2,
                                              arrest_table = nemsqar_arrest_table,
                                              response_table = nemsqar_response_table,
                                              vitals_table = non_numeric_data1,
                                              procedures_table = nemsqar_procedures_table,
                                              erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
                                              incident_date_col = `Incident Date`,
                                              patient_DOB_col = `Patient Date Of Birth (ePatient.17)`,
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

# Function should handle a dataframe as expected
testthat::test_that("airway_01_population handles a dataframe input", {

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
    evitals_06 = rep(c(90, 100, 102, 103, 104), 2),
    evitals_12 = rep(c(90, 91, 92, 93, 94), 2),
    earrest_01 = rep("No", 10),
    eprocedures_01 = rep(lubridate::as_datetime(c("2025-01-01 23:00:00",
                                                  "2025-01-05 12:00:00", "2025-02-01 19:00:00", "2025-01-01 05:00:00",
                                                  "2025-06-01 13:00:00")), 2),
    eprocedures_02 = rep("No", 10),
    eprocedures_03 = rep(c(16883004, 112798008, 78121007, 49077009,
                           673005), 2),
    eprocedures_05 = rep(1, 10),
    eprocedures_06 = rep(9923003, 10),

  )

  # Run the function
  result <- airway_01_population(df = data,
                      erecord_01_col = erecord_01,
                      incident_date_col = incident_date,
                      patient_DOB_col = patient_dob,
                      epatient_15_col = epatient_15,
                      epatient_16_col = epatient_16,
                      eresponse_05_col = eresponse_05,
                      eprocedures_01_col = eprocedures_01,
                      eprocedures_02_col = eprocedures_02,
                      eprocedures_03_col = eprocedures_03,
                      eprocedures_05_col = eprocedures_05,
                      eprocedures_06_col = eprocedures_06,
                      earrest_01_col = earrest_01,
                      evitals_01_col = evitals_01,
                      evitals_06_col = evitals_06,
                      evitals_12_col = evitals_12
  )

  testthat::expect_true(!is.logical(result$initial_population))
  testthat::expect_true(!is.logical(result$adults))
  testthat::expect_true(!is.logical(result$peds))
  testthat::expect_true(!is.logical(result$filter_process))
  testthat::expect_equal(result$filter_process |>
                           dplyr::filter(filter == "Total procedures in dataset") |>
                           dplyr::pull(count),
                         nrow(data) / 2
  )

})


