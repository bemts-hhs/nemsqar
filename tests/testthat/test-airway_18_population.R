testthat::test_that("Test for non-missing table and df arguments", {

# all NULL arguments to tables and df
# expect an error
  testthat::expect_error(
    airway_18_population(df = tibble::tibble(),
              patient_scene_table = tibble::tibble(),
              procedures_table = tibble::tibble(),
              vitals_table = tibble::tibble(),
              airway_table = tibble::tibble(),
              response_table = tibble::tibble(),
              erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
              incident_date_col = `Incident Date`,
              patient_DOB_col = `Patient Date Of Birth (ePatient.17)`,
              epatient_15_col = `Patient Age (ePatient.15)`,
              epatient_16_col = `Patient Age Units (ePatient.16)`,
              eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
              eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
              eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
              eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`,
              eprocedures_06_col = `Procedure Successful (eProcedures.06)`,
              eairway_02_col = `Airway Device Placement Confirmation Date Time (eAirway.02)`,
              eairway_04_col = `Airway Device Placement Confirmed Method List (eAirway.04)`,
              evitals_01_col = `Vitals Signs Taken Date Time (eVitals.01)`,
              evitals_16_col = `Vitals Carbon Dioxide CO2 (eVitals.16)`
              ),
    regexp = "Please choose"
  )

})

testthat::test_that("Test for df argument with wrong format", {

  # all NULL arguments to tables and df
  # expect an error
  testthat::expect_error(
    airway_18_population(df = list(),
                         patient_scene_table = NULL,
                         procedures_table = NULL,
                         vitals_table = NULL,
                         airway_table = NULL,
                         response_table = NULL,
                         erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
                         incident_date_col = `Incident Date`,
                         patient_DOB_col = `Patient Date Of Birth (ePatient.17)`,
                         epatient_15_col = `Patient Age (ePatient.15)`,
                         epatient_16_col = `Patient Age Units (ePatient.16)`,
                         eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
                         eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
                         eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
                         eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`,
                         eprocedures_06_col = `Procedure Successful (eProcedures.06)`,
                         eairway_02_col = `Airway Device Placement Confirmation Date Time (eAirway.02)`,
                         eairway_04_col = `Airway Device Placement Confirmed Method List (eAirway.04)`,
                         evitals_01_col = `Vitals Signs Taken Date Time (eVitals.01)`,
                         evitals_16_col = `Vitals Carbon Dioxide CO2 (eVitals.16)`
    ),
    regexp = "An object of class"
  )

})

testthat::test_that("Test for incorrectly formatted DOB / incident date", {
  # Load necessary test data
  data("nemsqar_airway_table")
  data("nemsqar_patient_scene_table")
  data("nemsqar_response_table")
  data("nemsqar_vitals_table")
  data("nemsqar_procedures_table")

  wrong_data <- nemsqar_patient_scene_table
  wrong_data$`Incident Date` <- as.character(nemsqar_patient_scene_table$`Incident Date`)
  wrong_data$`Patient Date Of Birth (ePatient.17)` <- as.character(nemsqar_patient_scene_table$`Patient Date Of Birth (ePatient.17)`)

  # all NULL arguments to tables and df
  # expect an error
  testthat::expect_error(
    airway_18_population(
      df = NULL,
      patient_scene_table = wrong_data,
      procedures_table = nemsqar_procedures_table,
      vitals_table = nemsqar_vitals_table,
      airway_table = nemsqar_airway_table,
      response_table = nemsqar_response_table,
      erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
      incident_date_col = `Incident Date`,
      patient_DOB_col = `Patient Date Of Birth (ePatient.17)`,
      epatient_15_col = `Patient Age (ePatient.15)`,
      epatient_16_col = `Patient Age Units (ePatient.16)`,
      eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
      eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
      eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
      eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`,
      eprocedures_06_col = `Procedure Successful (eProcedures.06)`,
      eairway_02_col = `Airway Device Placement Confirmation Date Time (eAirway.02)`,
      eairway_04_col = `Airway Device Placement Confirmed Method List (eAirway.04)`,
      evitals_01_col = `Vitals Signs Taken Date Time (eVitals.01)`,
      evitals_16_col = `Vitals Carbon Dioxide CO2 (eVitals.16)`
    ),
    regexp = "For the variables"
  )

})

testthat::test_that("Test for correctly formatted dates/times in df", {

  # get a big df with useful data
big_df <- tibble::tibble(
    `Incident Patient Care Report Number - PCR (eRecord.01)` = c("a", "b", "c", "d", "e"),
    `Incident Date` = c(
      as.Date("2023-01-01"),
      as.Date("2023-06-01"),
      as.Date("2023-05-01"),
      as.Date("2023-12-01"),
      as.Date("2023-12-01")
    ),     `Patient Date Of Birth (ePatient.17)` = c(
      as.Date("2005-01-01"),
      as.Date("2000-06-01"),
      as.Date("1995-05-01"),
      as.Date("2018-12-01"),
      as.Date("1970-12-01")
    ),     `Patient Age (ePatient.15)` = c(18, 23, 28, 5, 53),
    `Patient Age Units (ePatient.16)` = c("Years", "Years", "Years", "Years", "Years"),
    `Response Type Of Service Requested With Code (eResponse.05)` = rep("2205001", 5),
    `Procedure Performed Date Time (eProcedures.01)` = c(
      lubridate::as_datetime("2023-01-01 15:00:00"),
      lubridate::as_datetime("2023-06-01 14:00:00"),
      lubridate::as_datetime("2023-05-01 13:00:00"),
      lubridate::as_datetime("2023-12-01 12:00:00"),
      lubridate::as_datetime("2023-12-01 06:00:00")
    ),
    `Procedure Performed Prior To EMS Care (eProcedures.02)` = rep("No", 5),
    `Procedure Performed Description And Code (eProcedures.03)` = rep("112798008", 5),
    `Procedure Number Of Attempts (eProcedures.05)` = rep(1, 5),
    `Procedure Successful (eProcedures.06)` = rep("Yes", 5),
    `Airway Device Placement Confirmation Date Time (eAirway.02)` = c(
      lubridate::as_datetime("2023-01-01 15:05:00"),
      lubridate::as_datetime("2023-06-01 14:05:00"),
      lubridate::as_datetime("2023-05-01 13:05:00"),
      lubridate::as_datetime("2023-12-01 12:05:00"),
      lubridate::as_datetime("2023-12-01 06:05:00")
    ),
    `Airway Device Placement Confirmed Method List (eAirway.04)` = rep("4004019", 5),
    `Vitals Signs Taken Date Time (eVitals.01)` = c(
      lubridate::as_datetime("2023-01-01 15:03:00"),
      lubridate::as_datetime("2023-06-01 14:03:00"),
      lubridate::as_datetime("2023-05-01 13:03:00"),
      lubridate::as_datetime("2023-12-01 12:03:00"),
      lubridate::as_datetime("2023-12-01 06:03:00")
    ),
    `Vitals Carbon Dioxide CO2 (eVitals.16)` = rep(6, 5)
  )

  # incorrectly formatted date columns
  big_df_demo <- big_df |>
    dplyr::mutate(dplyr::across(matches("birth|incident date"), ~ as.character(.)
                                )
                  )

  big_df_dates <- big_df |>
    dplyr::mutate(dplyr::across(matches("date time"), ~ as.character(.))
                  )

  # all character arguments to incident date and
  # the patient DOB
  # expect an error
  testthat::expect_error(
    airway_18_population(
      df = big_df_demo,
      erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
      incident_date_col = `Incident Date`,
      patient_DOB_col = `Patient Date Of Birth (ePatient.17)`,
      epatient_15_col = `Patient Age (ePatient.15)`,
      epatient_16_col = `Patient Age Units (ePatient.16)`,
      eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
      eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
      eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
      eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`,
      eprocedures_06_col = `Procedure Successful (eProcedures.06)`,
      eairway_02_col = `Airway Device Placement Confirmation Date Time (eAirway.02)`,
      eairway_04_col = `Airway Device Placement Confirmed Method List (eAirway.04)`,
      evitals_01_col = `Vitals Signs Taken Date Time (eVitals.01)`,
      evitals_16_col = `Vitals Carbon Dioxide CO2 (eVitals.16)`
    ),
    regexp = "For the variables"
  )

  # all character arguments to vitals, airway, and
  # the procedures date time fields
  # expect an error
  testthat::expect_error(
    airway_18_population(
      df = big_df_dates,
      erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
      incident_date_col = `Incident Date`,
      patient_DOB_col = `Patient Date Of Birth (ePatient.17)`,
      epatient_15_col = `Patient Age (ePatient.15)`,
      epatient_16_col = `Patient Age Units (ePatient.16)`,
      eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
      eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
      eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
      eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`,
      eprocedures_06_col = `Procedure Successful (eProcedures.06)`,
      eairway_02_col = `Airway Device Placement Confirmation Date Time (eAirway.02)`,
      eairway_04_col = `Airway Device Placement Confirmed Method List (eAirway.04)`,
      evitals_01_col = `Vitals Signs Taken Date Time (eVitals.01)`,
      evitals_16_col = `Vitals Carbon Dioxide CO2 (eVitals.16)`
    ),
    regexp = "For the variables"
  )

  # all character arguments to vitals and
  # the procedures date time fields
  # expect an error
  testthat::expect_error(
    airway_18_population(
      df = big_df_dates,
      erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
      incident_date_col = `Incident Date`,
      patient_DOB_col = `Patient Date Of Birth (ePatient.17)`,
      epatient_15_col = `Patient Age (ePatient.15)`,
      epatient_16_col = `Patient Age Units (ePatient.16)`,
      eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
      eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
      eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
      eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`,
      eprocedures_06_col = `Procedure Successful (eProcedures.06)`,
      eairway_02_col = NULL,
      eairway_04_col = NULL,
      evitals_01_col = `Vitals Signs Taken Date Time (eVitals.01)`,
      evitals_16_col = `Vitals Carbon Dioxide CO2 (eVitals.16)`
    ),
    regexp = "For the variables"
  )



})

testthat::test_that("Ensure the function runs with the df method", {

  # get a big df with useful data
  big_df <- tibble::tibble(
    `Incident Patient Care Report Number - PCR (eRecord.01)` = c("a", "b", "c", "d", "e"),
    `Incident Date` = c(
      as.Date("2023-01-01"),
      as.Date("2023-06-01"),
      as.Date("2023-05-01"),
      as.Date("2023-12-01"),
      as.Date("2023-12-01")
    ),     `Patient Date Of Birth (ePatient.17)` = c(
      as.Date("2005-01-01"),
      as.Date("2000-06-01"),
      as.Date("1995-05-01"),
      as.Date("2018-12-01"),
      as.Date("1970-12-01")
    ),     `Patient Age (ePatient.15)` = c(18, 23, 28, 5, 53),
    `Patient Age Units (ePatient.16)` = c("Years", "Years", "Years", "Years", "Years"),
    `Response Type Of Service Requested With Code (eResponse.05)` = rep("2205001", 5),
    `Procedure Performed Date Time (eProcedures.01)` = c(
      lubridate::as_datetime("2023-01-01 15:00:00"),
      lubridate::as_datetime("2023-06-01 14:00:00"),
      lubridate::as_datetime("2023-05-01 13:00:00"),
      lubridate::as_datetime("2023-12-01 12:00:00"),
      lubridate::as_datetime("2023-12-01 06:00:00")
    ),
    `Procedure Performed Prior To EMS Care (eProcedures.02)` = rep("No", 5),
    `Procedure Performed Description And Code (eProcedures.03)` = rep("112798008", 5),
    `Procedure Number Of Attempts (eProcedures.05)` = rep(1, 5),
    `Procedure Successful (eProcedures.06)` = rep("Yes", 5),
    `Airway Device Placement Confirmation Date Time (eAirway.02)` = c(
      lubridate::as_datetime("2023-01-01 15:05:00"),
      lubridate::as_datetime("2023-06-01 14:05:00"),
      lubridate::as_datetime("2023-05-01 13:05:00"),
      lubridate::as_datetime("2023-12-01 12:05:00"),
      lubridate::as_datetime("2023-12-01 06:05:00")
    ),
    `Airway Device Placement Confirmed Method List (eAirway.04)` = rep("4004019", 5),
    `Vitals Signs Taken Date Time (eVitals.01)` = c(
      lubridate::as_datetime("2023-01-01 15:03:00"),
      lubridate::as_datetime("2023-06-01 14:03:00"),
      lubridate::as_datetime("2023-05-01 13:03:00"),
      lubridate::as_datetime("2023-12-01 12:03:00"),
      lubridate::as_datetime("2023-12-01 06:03:00")
    ),
    `Vitals Carbon Dioxide CO2 (eVitals.16)` = rep(6, 5)
  )

  # all arguments correct
  # expect success
  result <- airway_18_population(
      df = big_df,
      erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
      incident_date_col = `Incident Date`,
      patient_DOB_col = `Patient Date Of Birth (ePatient.17)`,
      epatient_15_col = `Patient Age (ePatient.15)`,
      epatient_16_col = `Patient Age Units (ePatient.16)`,
      eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
      eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
      eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
      eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`,
      eprocedures_06_col = `Procedure Successful (eProcedures.06)`,
      eairway_02_col = `Airway Device Placement Confirmation Date Time (eAirway.02)`,
      eairway_04_col = `Airway Device Placement Confirmed Method List (eAirway.04)`,
      evitals_01_col = `Vitals Signs Taken Date Time (eVitals.01)`,
      evitals_16_col = `Vitals Carbon Dioxide CO2 (eVitals.16)`
    )

  testthat::expect_type(result, "list")
  testthat::expect_equal(nrow(result$filter_process), 12)
  testthat::expect_equal(nrow(result$initial_population), 5)

})

testthat::test_that("Test for incorrectly formatted vitals/airway times", {
  # Load necessary test data
  data("nemsqar_airway_table")
  data("nemsqar_patient_scene_table")
  data("nemsqar_response_table")
  data("nemsqar_vitals_table")
  data("nemsqar_procedures_table")

  # get wrong data
  wrong_data_vitals <- nemsqar_vitals_table
  wrong_data_airway <- nemsqar_airway_table
  wrong_data_proc <- nemsqar_procedures_table
  wrong_data_vitals$`Vitals Signs Taken Date Time (eVitals.01)` <- as.character(nemsqar_vitals_table$`Vitals Signs Taken Date Time (eVitals.01)`)
  wrong_data_proc$`Procedure Performed Date Time (eProcedures.01)` <- as.character(nemsqar_procedures_table$`Procedure Performed Date Time (eProcedures.01)`)
  wrong_data_airway$`Airway Device Placement Confirmation Date Time (eAirway.02)` <- as.character(nemsqar_airway_table$`Airway Device Placement Confirmation Date Time (eAirway.02)`)

  # all NULL arguments to tables and df
  # expect an error
  testthat::expect_error(
    airway_18_population(
      df = NULL,
      patient_scene_table = nemsqar_patient_scene_table,
      procedures_table = wrong_data_proc,
      vitals_table = wrong_data_vitals,
      airway_table = wrong_data_airway,
      response_table = nemsqar_response_table,
      erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
      incident_date_col = `Incident Date`,
      patient_DOB_col = `Patient Date Of Birth (ePatient.17)`,
      epatient_15_col = `Patient Age (ePatient.15)`,
      epatient_16_col = `Patient Age Units (ePatient.16)`,
      eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
      eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
      eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
      eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`,
      eprocedures_06_col = `Procedure Successful (eProcedures.06)`,
      eairway_02_col = `Airway Device Placement Confirmation Date Time (eAirway.02)`,
      eairway_04_col = `Airway Device Placement Confirmed Method List (eAirway.04)`,
      evitals_01_col = `Vitals Signs Taken Date Time (eVitals.01)`,
      evitals_16_col = `Vitals Carbon Dioxide CO2 (eVitals.16)`
    ),
    regexp = "For the variables"
  )

})

testthat::test_that("Test for incorrectly formatted table argument", {
  # Load necessary test data
  data("nemsqar_airway_table")
  data("nemsqar_patient_scene_table")
  data("nemsqar_response_table")
  data("nemsqar_vitals_table")
  data("nemsqar_procedures_table")

  # all NULL arguments to tables and df
  # expect an error
  testthat::expect_error(
    airway_18_population(
      df = NULL,
      patient_scene_table = list(),
      procedures_table = nemsqar_procedures_table,
      vitals_table = nemsqar_vitals_table,
      airway_table = nemsqar_airway_table,
      response_table = nemsqar_response_table,
      erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
      incident_date_col = `Incident Date`,
      patient_DOB_col = `Patient Date Of Birth (ePatient.17)`,
      epatient_15_col = `Patient Age (ePatient.15)`,
      epatient_16_col = `Patient Age Units (ePatient.16)`,
      eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
      eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
      eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
      eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`,
      eprocedures_06_col = `Procedure Successful (eProcedures.06)`,
      eairway_02_col = `Airway Device Placement Confirmation Date Time (eAirway.02)`,
      eairway_04_col = `Airway Device Placement Confirmed Method List (eAirway.04)`,
      evitals_01_col = `Vitals Signs Taken Date Time (eVitals.01)`,
      evitals_16_col = `Vitals Carbon Dioxide CO2 (eVitals.16)`
    ),
    regexp = "One or more"
  )

})

testthat::test_that("Test for missing column names", {
  # Load necessary test data
  data("nemsqar_airway_table")
  data("nemsqar_patient_scene_table")
  data("nemsqar_response_table")
  data("nemsqar_vitals_table")
  data("nemsqar_procedures_table")

  testthat::expect_error(airway_18_population(
    df = NULL,
    patient_scene_table = nemsqar_patient_scene_table,
    procedures_table = nemsqar_procedures_table,
    vitals_table = nemsqar_vitals_table,
    airway_table = nemsqar_airway_table,
    response_table = nemsqar_response_table,
    erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`
  ), "arguments is missing")
})

testthat::test_that("Test for missing df and tables", {
  testthat::expect_error(airway_18_population(
    df = NULL,
    patient_scene_table = NULL,
    procedures_table = NULL,
    vitals_table = NULL,
    airway_table = NULL,
    response_table = NULL,
    erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
    incident_date_col = `Incident Date`,
    patient_DOB_col = `Patient Date Of Birth (ePatient.17)`,
    epatient_15_col = `Patient Age (ePatient.15)`,
    epatient_16_col = `Patient Age Units (ePatient.16)`,
    eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
    eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
    eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
    eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`,
    eprocedures_06_col = `Procedure Successful (eProcedures.06)`,
    eairway_02_col = `Airway Device Placement Confirmation Date Time (eAirway.02)`,
    eairway_04_col = `Airway Device Placement Confirmed Method List (eAirway.04)`,
    evitals_01_col = `Vitals Signs Taken Date Time (eVitals.01)`,
    evitals_16_col = `Vitals Carbon Dioxide CO2 (eVitals.16)`
  ))
})

testthat::test_that("Test for correct input handling (table method)", {

  # Mock data setup
  patient_scene_table <- tibble::tibble(
    erecord_01 = 1:5,
    incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01", "2025-01-01", "2025-06-01")),
    patient_dob = as.Date(c("2000-01-01", "2020-01-01", "2023-02-01", "2023-01-01", "1970-06-01")),
    epatient_15 = c(25, 5, 2, 2, 55),  # Ages
    epatient_16 = rep("Years", 5)
  )

  procedures_table <- tibble::tibble(
    erecord_01 = 1:5,
    eprocedures_01 = Sys.time() - c(1, 3, 5, 7, 9),
    eprocedures_02 = rep("No", 5),
    eprocedures_03 = rep("44738004", 5),
    eprocedures_05 = c(1, 1, 1, 1, 1),
    eprocedures_06 = rep("Yes", 5)
  )

  response_table <- tibble::tibble(
    erecord_01 = 1:5,
    eresponse_05 = rep("2205001", 5)
  )

  vitals_table <- tibble::tibble(
    erecord_01 = 1:5,
    evitals_01 = Sys.time() + c(0, 1, 2, 3, 4),
    evitals_16 = c(6, 5, 8, 7, 9)
  )

  airway_table <- tibble::tibble(
    erecord_01 = 1:5,
    eairway_02 = Sys.time() + c(1, 3, 5, 7, 9),
    eairway_04 = rep("4004019", 5)
  )

  result <- airway_18_population(
    df = NULL,
    patient_scene_table = patient_scene_table,
    procedures_table = procedures_table,
    vitals_table = vitals_table,
    airway_table = airway_table,
    response_table = response_table,
    erecord_01_col = erecord_01,
    incident_date_col = incident_date,
    patient_DOB_col = patient_dob,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    eprocedures_01_col = eprocedures_01,
    eprocedures_02_col = eprocedures_02,
    eprocedures_03_col = eprocedures_03,
    eprocedures_06_col = eprocedures_06,
    eairway_02_col = eairway_02,
    eairway_04_col = eairway_04,
    evitals_01_col = evitals_01,
    evitals_16_col = evitals_16
  )

  testthat::expect_type(result, "list")
  testthat::expect_true(all(names(result) %in% c("filter_process", "adults", "peds", "initial_population", "computing_population")))

})

testthat::test_that("Test for invalid column names", {
  testthat::expect_error(airway_18_population(
    df = nemsqar_airway_18_df,
    patient_scene_table = NULL,
    procedures_table = NULL,
    vitals_table = NULL,
    airway_table = NULL,
    response_table = NULL,
    erecord_01_col = `Invalid Column Name`,
    incident_date_col = `Incident Date`,
    patient_DOB_col = `Patient Date Of Birth (ePatient.17)`,
    epatient_15_col = `Patient Age (ePatient.15)`,
    epatient_16_col = `Patient Age Units (ePatient.16)`,
    eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
    eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
    eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
    eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`,
    eprocedures_06_col = `Procedure Successful (eProcedures.06)`,
    eairway_02_col = `Airway Device Placement Confirmation Date Time (eAirway.02)`,
    eairway_04_col = `Airway Device Placement Confirmed Method List (eAirway.04)`,
    evitals_01_col = `Vitals Signs Taken Date Time (eVitals.01)`,
    evitals_16_col = `Vitals Carbon Dioxide CO2 (eVitals.16)`
  ))
})

testthat::test_that("Test for table and df input conflicts", {

  df <- tibble::tibble()

  testthat::expect_error(airway_18_population(
    df = nemsqar_airway_18_df,
    patient_scene_table = nemsqar_patient_scene_table,
    procedures_table = nemsqar_procedures_table,
    vitals_table = nemsqar_vitals_table,
    airway_table = nemsqar_airway_table,
    response_table = nemsqar_response_table,
    erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
    incident_date_col = `Incident Date`,
    patient_DOB_col = `Patient Date Of Birth (ePatient.17)`,
    epatient_15_col = `Patient Age (ePatient.15)`,
    epatient_16_col = `Patient Age Units (ePatient.16)`,
    eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
    eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
    eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
    eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`,
    eprocedures_06_col = `Procedure Successful (eProcedures.06)`,
    eairway_02_col = `Airway Device Placement Confirmation Date Time (eAirway.02)`,
    eairway_04_col = `Airway Device Placement Confirmed Method List (eAirway.04)`,
    evitals_01_col = `Vitals Signs Taken Date Time (eVitals.01)`,
    evitals_16_col = `Vitals Carbon Dioxide CO2 (eVitals.16)`
  ))
})

# Example test: Check if intubation data is correctly filtered
testthat::test_that("incident date is correctly validated", {

  # Mock data setup
  patient_scene_table <- tibble::tibble(
    erecord_01_col = 1:5,
    incident_date_col = c(5, 10, 15, 20, 25),
    patient_DOB_col = c(20, 25, 30, 35, 40),
    epatient_15 = c(20, 30, 40, 50, 60),
    epatient_16 = c("Years", "Months", "Days", "Years", "Months")
  )

  procedures_table <- tibble::tibble(
    erecord_01_col = 1:5,
    eprocedures_01_col = Sys.time() - c(1, 3, 5, 7, 9),
    eprocedures_02_col = c("Yes", "No", "Yes", "No", "Yes"),
    eprocedures_03_col = c("endotracheal intubation", "other", "endotracheal intubation", "other", "endotracheal intubation"),
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
    patient_DOB_col = patient_DOB_col,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05_col,
    eprocedures_01_col = eprocedures_01_col,
    eprocedures_02_col = eprocedures_02_col,
    eprocedures_03_col = eprocedures_03_col,
    eprocedures_06_col = eprocedures_06_col,
    eairway_02_col = eairway_02_col,
    eairway_04_col = eairway_04_col,
    evitals_01_col = evitals_01_col,
    evitals_16_col = evitals_16_col
  ))

})

# Test case 1: Verify patient age calculation when both dates are provided
testthat::test_that("vitals times are correctly validated", {

  # Mock data setup
  patient_scene_table <- tibble::tibble(
    erecord_01_col = 1:5,
    incident_date_col = Sys.time() - c(5, 10, 15, 20, 25),
    patient_DOB_col = Sys.time() - c(1120, 1125, 1130, 1135, 1140),
    epatient_15 = c(20, 30, 40, 50, 60),
    epatient_16 = c("Years", "Months", "Days", "Years", "Months")
  )

  procedures_table <- tibble::tibble(
    erecord_01_col = 1:5,
    eprocedures_01_col = Sys.time() - c(1, 3, 5, 7, 9),
    eprocedures_02_col = c("Yes", "No", "Yes", "No", "Yes"),
    eprocedures_03_col = c("endotracheal intubation", "other", "endotracheal intubation", "other", "endotracheal intubation"),
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
    patient_DOB_col = patient_DOB_col,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05_col,
    eprocedures_01_col = eprocedures_01_col,
    eprocedures_02_col = eprocedures_02_col,
    eprocedures_03_col = eprocedures_03_col,
    eprocedures_06_col = eprocedures_06_col,
    eairway_02_col = eairway_02_col,
    eairway_04_col = eairway_04_col,
    evitals_01_col = evitals_01_col,
    evitals_16_col = evitals_16_col
  ))


})

# Test case 2: Check if the numerator data extraction works
testthat::test_that("denominator data is correctly extracted", {

  # Mock data setup
  patient_scene_table <- tibble::tibble(
    erecord_01 = 1:5,
    incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01", "2025-01-01", "2025-06-01")),
    patient_dob = as.Date(c("2000-01-01", "2020-01-01", "2023-02-01", "2023-01-01", "1970-06-01")),
    epatient_15 = c(25, 5, 2, 2, 55),  # Ages
    epatient_16 = rep("Years", 5)
  )

  procedures_table <- tibble::tibble(
    erecord_01 = 1:5,
    eprocedures_01 = Sys.time() - c(1, 3, 5, 7, 9),
    eprocedures_02 = rep("No", 5),
    eprocedures_03 = rep("44738004", 5),
    eprocedures_05 = c(1, 1, 1, 1, 1),
    eprocedures_06 = rep("Yes", 5)
  )

  response_table <- tibble::tibble(
    erecord_01 = 1:5,
    eresponse_05 = rep("2205001", 5)
  )

  vitals_table <- tibble::tibble(
    erecord_01 = 1:5,
    evitals_01 = Sys.time() + c(0, 1, 2, 3, 4),
    evitals_16 = c(6, 5, 8, 7, 9)
  )

  airway_table <- tibble::tibble(
    erecord_01 = 1:5,
    eairway_02 = Sys.time() + c(1, 3, 5, 7, 9),
    eairway_04 = rep("4004019", 5)
  )

  result <- airway_18_population(
    patient_scene_table = patient_scene_table,
    procedures_table = procedures_table,
    vitals_table = vitals_table,
    airway_table = airway_table,
    response_table = response_table,
    erecord_01_col = erecord_01,
    incident_date_col = incident_date,
    patient_DOB_col = patient_dob,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    eprocedures_01_col = eprocedures_01,
    eprocedures_02_col = eprocedures_02,
    eprocedures_03_col = eprocedures_03,
    eprocedures_06_col = eprocedures_06,
    eairway_02_col = eairway_02,
    eairway_04_col = eairway_04,
    evitals_01_col = evitals_01,
    evitals_16_col = evitals_16
  )

  filters <- result$filter_process

  testthat::expect_equal(nrow(filters), 12)

})
