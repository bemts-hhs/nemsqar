testthat::test_that("airway_18 returns expected structure with df argument", {

  # get a big df with useful data
  big_df <- tibble::tibble(
    `Incident Patient Care Report Number - PCR (eRecord.01)` = c("a", "b", "c", "d", "e"),
    `Incident Date` = c(
      as.Date("2023-01-01"),
      as.Date("2023-06-01"),
      as.Date("2023-05-01"),
      as.Date("2023-12-01"),
      as.Date("2023-12-01")
    ),
    `Patient Date Of Birth (ePatient.17)` = c(
      as.Date("2005-01-01"),
      as.Date("2000-06-01"),
      as.Date("1995-05-01"),
      as.Date("2018-12-01"),
      as.Date("1970-12-01")
    ),
    `Patient Age (ePatient.15)` = c(18, 23, 28, 5, 53),
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

  result <- airway_18(df = big_df,
                      erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
                      incident_date_col = `Incident Date`,
                      patient_dob_col = `Patient Date Of Birth (ePatient.17)`,
                      epatient_15_col = `Patient Age (ePatient.15)`,
                      epatient_16_col = `Patient Age Units (ePatient.16)`,
                      eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
                      evitals_01_col = `Vitals Signs Taken Date Time (eVitals.01)`,
                      evitals_16_col = `Vitals Carbon Dioxide CO2 (eVitals.16)`,
                      eairway_02_col = `Airway Device Placement Confirmation Date Time (eAirway.02)`,
                      eairway_04_col = `Airway Device Placement Confirmed Method List (eAirway.04)`,
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

testthat::test_that("airway_18 returns expected structure with table args", {

  # get tables with useful data
  patient_table <- tibble::tibble(
    `Incident Patient Care Report Number - PCR (eRecord.01)` = c("a", "b", "c", "d", "e"),
    `Incident Date` = c(
      as.Date("2023-01-01"),
      as.Date("2023-06-01"),
      as.Date("2023-05-01"),
      as.Date("2023-12-01"),
      as.Date("2023-12-01")
    ),
    `Patient Date Of Birth (ePatient.17)` = c(
      as.Date("2005-01-01"),
      as.Date("2000-06-01"),
      as.Date("1995-05-01"),
      as.Date("2018-12-01"),
      as.Date("1970-12-01")
    ),
    `Patient Age (ePatient.15)` = c(18, 23, 28, 5, 53),
    `Patient Age Units (ePatient.16)` = c("Years", "Years", "Years", "Years", "Years")
    )

  response_table <- tibble::tibble(
    `Incident Patient Care Report Number - PCR (eRecord.01)` = c("a", "b", "c", "d", "e"),

    `Response Type Of Service Requested With Code (eResponse.05)` = rep("2205001", 5)
    )

  procedures_table <- tibble::tibble(
    `Incident Patient Care Report Number - PCR (eRecord.01)` = c("a", "b", "c", "d", "e"),

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
    `Procedure Successful (eProcedures.06)` = rep("Yes", 5)
    )

  airway_table <- tibble::tibble(
    `Incident Patient Care Report Number - PCR (eRecord.01)` = c("a", "b", "c", "d", "e"),

    `Airway Device Placement Confirmation Date Time (eAirway.02)` = c(
      lubridate::as_datetime("2023-01-01 15:05:00"),
      lubridate::as_datetime("2023-06-01 14:05:00"),
      lubridate::as_datetime("2023-05-01 13:05:00"),
      lubridate::as_datetime("2023-12-01 12:05:00"),
      lubridate::as_datetime("2023-12-01 06:05:00")
    ),
    `Airway Device Placement Confirmed Method List (eAirway.04)` = rep("4004019", 5)
    )

  vitals_table <- tibble::tibble(
    `Incident Patient Care Report Number - PCR (eRecord.01)` = c("a", "b", "c", "d", "e"),

    `Vitals Signs Taken Date Time (eVitals.01)` = c(
      lubridate::as_datetime("2023-01-01 15:03:00"),
      lubridate::as_datetime("2023-06-01 14:03:00"),
      lubridate::as_datetime("2023-05-01 13:03:00"),
      lubridate::as_datetime("2023-12-01 12:03:00"),
      lubridate::as_datetime("2023-12-01 06:03:00")
    ),
    `Vitals Carbon Dioxide CO2 (eVitals.16)` = rep(6, 5)
  )

  result <- airway_18(patient_scene_table = patient_table,
                      response_table = response_table,
                      airway_table = airway_table,
                      vitals_table = vitals_table,
                      procedures_table = procedures_table,
                      erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
                      incident_date_col = `Incident Date`,
                      patient_dob_col = `Patient Date Of Birth (ePatient.17)`,
                      epatient_15_col = `Patient Age (ePatient.15)`,
                      epatient_16_col = `Patient Age Units (ePatient.16)`,
                      eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
                      evitals_01_col = `Vitals Signs Taken Date Time (eVitals.01)`,
                      evitals_16_col = `Vitals Carbon Dioxide CO2 (eVitals.16)`,
                      eairway_02_col = `Airway Device Placement Confirmation Date Time (eAirway.02)`,
                      eairway_04_col = `Airway Device Placement Confirmed Method List (eAirway.04)`,
                      eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
                      eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
                      eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`,
                      eprocedures_05_col = `Procedure Number Of Attempts (eProcedures.05)`,
                      eprocedures_06_col = `Procedure Successful (eProcedures.06)`
  )

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_equal(nrow(result), 2)
  testthat::expect_true(length(names(result)) == 6)

})

testthat::test_that("airway_18 correctly identifies advanced airway attempts", {

  # get tables with useful data
  patient_table <- tibble::tibble(
    `Incident Patient Care Report Number - PCR (eRecord.01)` = c("a", "b", "c", "d", "e"),
    `Incident Date` = c(
      as.Date("2023-01-01"),
      as.Date("2023-06-01"),
      as.Date("2023-05-01"),
      as.Date("2023-12-01"),
      as.Date("2023-12-01")
    ),
    `Patient Date Of Birth (ePatient.17)` = c(
      as.Date("2005-01-01"),
      as.Date("2000-06-01"),
      as.Date("1995-05-01"),
      as.Date("2018-12-01"),
      as.Date("1970-12-01")
    ),
    `Patient Age (ePatient.15)` = c(18, 23, 28, 5, 53),
    `Patient Age Units (ePatient.16)` = c("Years", "Years", "Years", "Years", "Years")
  )

  response_table <- tibble::tibble(
    `Incident Patient Care Report Number - PCR (eRecord.01)` = c("a", "b", "c", "d", "e"),

    `Response Type Of Service Requested With Code (eResponse.05)` = rep("2205001", 5)
  )

  procedures_table <- tibble::tibble(
    `Incident Patient Care Report Number - PCR (eRecord.01)` = c("a", "b", "c", "d", "e"),

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
    `Procedure Successful (eProcedures.06)` = rep("Yes", 5)
  )

  airway_table <- tibble::tibble(
    `Incident Patient Care Report Number - PCR (eRecord.01)` = c("a", "b", "c", "d", "e"),

    `Airway Device Placement Confirmation Date Time (eAirway.02)` = c(
      lubridate::as_datetime("2023-01-01 15:05:00"),
      lubridate::as_datetime("2023-06-01 14:05:00"),
      lubridate::as_datetime("2023-05-01 13:05:00"),
      lubridate::as_datetime("2023-12-01 12:05:00"),
      lubridate::as_datetime("2023-12-01 06:05:00")
    ),
    `Airway Device Placement Confirmed Method List (eAirway.04)` = rep("4004019", 5)
  )

  vitals_table <- tibble::tibble(
    `Incident Patient Care Report Number - PCR (eRecord.01)` = c("a", "b", "c", "d", "e"),

    `Vitals Signs Taken Date Time (eVitals.01)` = c(
      lubridate::as_datetime("2023-01-01 15:03:00"),
      lubridate::as_datetime("2023-06-01 14:03:00"),
      lubridate::as_datetime("2023-05-01 13:03:00"),
      lubridate::as_datetime("2023-12-01 12:03:00"),
      lubridate::as_datetime("2023-12-01 06:03:00")
    ),
    `Vitals Carbon Dioxide CO2 (eVitals.16)` = rep(6, 5)
  )

  result <- airway_18(patient_scene_table = patient_table,
                      response_table = response_table,
                      airway_table = airway_table,
                      vitals_table = vitals_table,
                      procedures_table = procedures_table,
                      erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
                      incident_date_col = `Incident Date`,
                      patient_dob_col = `Patient Date Of Birth (ePatient.17)`,
                      epatient_15_col = `Patient Age (ePatient.15)`,
                      epatient_16_col = `Patient Age Units (ePatient.16)`,
                      eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
                      evitals_01_col = `Vitals Signs Taken Date Time (eVitals.01)`,
                      evitals_16_col = `Vitals Carbon Dioxide CO2 (eVitals.16)`,
                      eairway_02_col = `Airway Device Placement Confirmation Date Time (eAirway.02)`,
                      eairway_04_col = `Airway Device Placement Confirmed Method List (eAirway.04)`,
                      eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
                      eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
                      eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`,
                      eprocedures_05_col = `Procedure Number Of Attempts (eProcedures.05)`,
                      eprocedures_06_col = `Procedure Successful (eProcedures.06)`
                      )

  result_pop <- airway_18_population(patient_scene_table = patient_table,
                                     response_table = response_table,
                                     airway_table = airway_table,
                                     vitals_table = vitals_table,
                                     procedures_table = procedures_table,
                                     erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
                                     incident_date_col = `Incident Date`,
                                     patient_dob_col = `Patient Date Of Birth (ePatient.17)`,
                                     epatient_15_col = `Patient Age (ePatient.15)`,
                                     epatient_16_col = `Patient Age Units (ePatient.16)`,
                                     eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
                                     evitals_01_col = `Vitals Signs Taken Date Time (eVitals.01)`,
                                     evitals_16_col = `Vitals Carbon Dioxide CO2 (eVitals.16)`,
                                     eairway_02_col = `Airway Device Placement Confirmation Date Time (eAirway.02)`,
                                     eairway_04_col = `Airway Device Placement Confirmed Method List (eAirway.04)`,
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
