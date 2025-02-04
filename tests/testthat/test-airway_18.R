testthat::test_that("airway_18 returns expected structure", {
  data("nemsqar_patient_scene_table", package = "nemsqar")
  data("nemsqar_airway_table", package = "nemsqar")
  data("nemsqar_response_table", package = "nemsqar")
  data("nemsqar_vitals_table", package = "nemsqar")
  data("nemsqar_procedures_table", package = "nemsqar")

  result <- airway_18(patient_scene_table = nemsqar_patient_scene_table,
                      response_table = nemsqar_response_table,
                      airway_table = nemsqar_airway_table,
                      vitals_table = nemsqar_vitals_table,
                      procedures_table = nemsqar_procedures_table,
                      erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
                      incident_date_col = `Incident Date`,patient_dob_col = `Patient Date Of Birth (ePatient.17)`,
                      epatient_15_col = `Patient Age (ePatient.15)`,
                      epatient_16_col = `Patient Age Units (ePatient.16)`,
                      eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
                      evitals_01_col = `Vitals Signs Taken Date Time (eVitals.01)`,
                      evitals_16_col = `Vitals Carbon Dioxide CO2 (eVitals.16)`,
                      eairway_02_col = `Airway Device Placement Confirmation Date Time (eAirway.02)`,
                      eairway_04_col = `Airway Device Placement Confirmed Method (eAirway.04)`,
                      eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
                      eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
                      eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`,
                      eprocedures_05_col = `Procedure Number Of Attempts (eProcedures.05)`,
                      eprocedures_06_col = `Procedure Successful (eProcedures.06)`
                      )

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_equal(nrow(result), 2)
  testthat::expect_true("Airway-18" %in% result$measure)

})

testthat::test_that("airway_18 correctly identifies advanced airway attempts", {
  data("nemsqar_patient_scene_table", package = "nemsqar")
  data("nemsqar_airway_table", package = "nemsqar")
  data("nemsqar_response_table", package = "nemsqar")
  data("nemsqar_vitals_table", package = "nemsqar")
  data("nemsqar_procedures_table", package = "nemsqar")

  result <- airway_18(patient_scene_table = nemsqar_patient_scene_table,
                      response_table = nemsqar_response_table,
                      airway_table = nemsqar_airway_table,
                      vitals_table = nemsqar_vitals_table,
                      procedures_table = nemsqar_procedures_table,
                      erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
                      incident_date_col = `Incident Date`,patient_dob_col = `Patient Date Of Birth (ePatient.17)`,
                      epatient_15_col = `Patient Age (ePatient.15)`,
                      epatient_16_col = `Patient Age Units (ePatient.16)`,
                      eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
                      evitals_01_col = `Vitals Signs Taken Date Time (eVitals.01)`,
                      evitals_16_col = `Vitals Carbon Dioxide CO2 (eVitals.16)`,
                      eairway_02_col = `Airway Device Placement Confirmation Date Time (eAirway.02)`,
                      eairway_04_col = `Airway Device Placement Confirmed Method (eAirway.04)`,
                      eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
                      eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
                      eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`,
                      eprocedures_05_col = `Procedure Number Of Attempts (eProcedures.05)`,
                      eprocedures_06_col = `Procedure Successful (eProcedures.06)`
                      )

  result_pop <- airway_18_population(patient_scene_table = nemsqar_patient_scene_table,
                      response_table = nemsqar_response_table,
                      airway_table = nemsqar_airway_table,
                      vitals_table = nemsqar_vitals_table,
                      procedures_table = nemsqar_procedures_table,
                      erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
                      incident_date_col = `Incident Date`,
                      patient_dob_col = `Patient Date Of Birth (ePatient.17)`,
                      epatient_15_col = `Patient Age (ePatient.15)`,
                      epatient_16_col = `Patient Age Units (ePatient.16)`,
                      eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
                      evitals_01_col = `Vitals Signs Taken Date Time (eVitals.01)`,
                      evitals_16_col = `Vitals Carbon Dioxide CO2 (eVitals.16)`,
                      eairway_02_col = `Airway Device Placement Confirmation Date Time (eAirway.02)`,
                      eairway_04_col = `Airway Device Placement Confirmed Method (eAirway.04)`,
                      eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
                      eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
                      eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`,
                      eprocedures_05_col = `Procedure Number Of Attempts (eProcedures.05)`,
                      eprocedures_06_col = `Procedure Successful (eProcedures.06)`
                      )

  adult_denom <- result |>
    dplyr::filter(pop == "Adult") |>
    dplyr::pull(denominator)

  population_adult_denom <- result_pop$filter_process |>
    dplyr::filter(filter == "Adults denominator") |>
    dplyr::pull(count)

  testthat::expect_equal(adult_denom, population_adult_denom)
})

testthat::test_that("airway_18 handles missing arguments", {
  df <- tibble::tibble(
    patient_id = 1:3,
    eProcedures.06 = c(NA, NA, NA)
  )

  testthat::expect_error(airway_18(df, erecord_01_col = patient_id, eprocedures_06_col = eProcedures.06))

})

testthat::test_that("airway_18 handles unexpected column names", {
  df <- tibble::tibble(
    patient_id = 1:3,
    custom_col = c("4102003", "4102001", "4102003")
  )

  testthat::expect_error(airway_18(df, custom_col = custom_col))
})

testthat::test_that("airway_18 handles empty input data", {
  df <- tibble::tibble(
    patient_id = integer(),
    eProcedures.06 = character(),
    epatient_15_col = character(),
    epatient_16_col = character(),
    eresponse_05_col = character(),
    eprocedures_01_col = character(),
    eprocedures_02_col = character(),
    eprocedures_03_col = character(),
    eprocedures_05_col = character(),
    eprocedures_06_col = character(),
    eairway_02_col = character(),
    eairway_04_col = character(),
    evitals_01_col = character(),
    evitals_16_col = character()
  )

  testthat::expect_error(airway_18(df, erecord_01_col = patient_id,
                      incident_date_col = NULL,
                      patient_dob_col = NULL,
                      epatient_15_col = epatient_15_col,
                      epatient_16_col = epatient_16_col,
                      eresponse_05_col = eresponse_05_col,
                      eprocedures_01_col = eprocedures_01_col,
                      eprocedures_02_col = eprocedures_02_col,
                      eprocedures_03_col = eprocedures_03_col,
                      eprocedures_05_col = eprocedures_05_col,
                      eprocedures_06_col = eprocedures_06_col,
                      eairway_02_col = NULL,
                      eairway_04_col = NULL,
                      evitals_01_col = evitals_01_col,
                      evitals_16_col = evitals_16_col
                      ))

})
