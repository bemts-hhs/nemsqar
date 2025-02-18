testthat::test_that("trauma_03_population rejects invalid argument combinations", {

  testthat::expect_error(trauma_03_population(df = tibble::tibble(),
                                              patient_scene_table = tibble::tibble(),
                                              response_table = tibble::tibble(),
                                              situation_table = tibble::tibble(),
                                              vitals_table = tibble::tibble(),
                                              disposition_table = tibble::tibble(),
                                              erecord_01_col = character(),
                                              epatient_15_col = numeric(),
                                              epatient_16_col = character(),
                                              eresponse_05_col = character(),
                                              esituation_02_col = character(),
                                              evitals_01_col = date(),
                                              evitals_27_initial_col = numeric(),
                                              evitals_27_last_col = numeric(),
                                              evitals_27_col = NULL,
                                              edisposition_28_col = character(),
                                              transport_disposition_col = character()
                                              ),
                         "requires either a"
                         )

  testthat::expect_error(trauma_03_population(df = tibble::tibble(),
                                              patient_scene_table = tibble::tibble(),
                                              response_table = tibble::tibble(),
                                              situation_table = tibble::tibble(),
                                              vitals_table = tibble::tibble(),
                                              disposition_table = tibble::tibble(),
                                              erecord_01_col = character(),
                                              epatient_15_col = numeric(),
                                              epatient_16_col = character(),
                                              eresponse_05_col = character(),
                                              esituation_02_col = character(),
                                              evitals_01_col = date(),
                                              evitals_27_initial_col = NULL,
                                              evitals_27_last_col = NULL,
                                              evitals_27_col = numeric(),
                                              edisposition_28_col = character(),
                                              transport_disposition_col = character()
                                              ),
                         "requires either a"
                         )

  testthat::expect_error(trauma_03_population(patient_scene_table = list(),
                                              response_table = tibble::tibble(),
                                              situation_table = tibble::tibble(),
                                              vitals_table = tibble::tibble(),
                                              disposition_table = tibble::tibble(),
                                              erecord_01_col = character(),
                                              epatient_15_col = numeric(),
                                              epatient_16_col = character(),
                                              incident_date_col = date(),
                                              patient_DOB_col = date(),
                                              eresponse_05_col = character(),
                                              esituation_02_col = character(),
                                              evitals_01_col = date(),
                                              evitals_27_initial_col = numeric(),
                                              evitals_27_last_col = numeric(),
                                              evitals_27_col = NULL,
                                              edisposition_28_col = character(),
                                              transport_disposition_col = character()
                                              ),
                         "One or more of the tables passed to"
                         )

  testthat::expect_error(trauma_03_population(patient_scene_table = list(),
                                              response_table = tibble::tibble(),
                                              situation_table = tibble::tibble(),
                                              vitals_table = tibble::tibble(),
                                              disposition_table = tibble::tibble(),
                                              erecord_01_col = character(),
                                              epatient_15_col = numeric(),
                                              epatient_16_col = character(),
                                              incident_date_col = date(),
                                              patient_DOB_col = date(),
                                              eresponse_05_col = character(),
                                              esituation_02_col = character(),
                                              evitals_01_col = date(),
                                              evitals_27_initial_col = NULL,
                                              evitals_27_last_col = NULL,
                                              evitals_27_col = numeric(),
                                              edisposition_28_col = character(),
                                              transport_disposition_col = character()
                                              ),
                         "One or more of the tables passed to"
                         )

})

testthat::test_that("trauma_03_population rejects missing required column arguments", {
  testthat::expect_error(trauma_03_population(df = tibble::tibble(), epatient_15_col = "Age"),
                         "One or more of the \\*_col arguments is missing")
})

testthat::test_that("trauma_03_population rejects non-dataframe inputs", {
  testthat::expect_error(trauma_03_population(df = list()),
                         "One or more")

  testthat::expect_error(trauma_03_population(patient_scene_table = matrix()),
                         "One or more")
})

testthat::test_that("trauma_03_population validates date column formats", {

  df_1 <- tibble::tibble(erecord_01_col = character(),
                       epatient_15_col = numeric(),
                       epatient_16_col = character(),
                       incident_date_col = character(),
                       patient_DOB_col = character(),
                       eresponse_05_col = character(),
                       esituation_02_col = character(),
                       evitals_01_col = date(),
                       evitals_27_initial_col = NULL,
                       evitals_27_last_col = NULL,
                       evitals_27_col = numeric(),
                       edisposition_28_col = character(),
                       transport_disposition_col = character()
                       )

  df_2 <- tibble::tibble(erecord_01_col = character(),
                         epatient_15_col = numeric(),
                         epatient_16_col = character(),
                         incident_date_col = character(),
                         patient_DOB_col = character(),
                         eresponse_05_col = character(),
                         esituation_02_col = character(),
                         evitals_01_col = date(),
                         evitals_27_initial_col = numeric(),
                         evitals_27_last_col = numeric(),
                         evitals_27_col = NULL,
                         edisposition_28_col = character(),
                         transport_disposition_col = character()
                         )

  testthat::expect_error(
    trauma_03_population(
      df_1,
      erecord_01_col = erecord_01,
      epatient_15_col = epatient_15,
      epatient_16_col = epatient_16,
      eresponse_05_col = eresponse_05,
      esituation_02_col = esituation_02,
      evitals_01_col = evitals_01,
      evitals_27_initial_col = NULL,
      evitals_27_last_col = NULL,
      evitals_27_col = evitals_27,
      edisposition_28_col = edisposition_28,
      transport_disposition_col = edisposition_30
    )
  )

  testthat::expect_error(
    trauma_03_population(
      df_2,
      erecord_01_col = erecord_01,
      epatient_15_col = epatient_15,
      epatient_16_col = epatient_16,
      eresponse_05_col = eresponse_05,
      esituation_02_col = esituation_02,
      evitals_01_col = evitals_01,
      evitals_27_initial_col = evitals_27_1,
      evitals_27_last_col = evitals_27_2,
      evitals_27_col = NULL,
      edisposition_28_col = edisposition_28,
      transport_disposition_col = edisposition_30
    )
  )

  testthat::expect_error(
    trauma_03_population(
      patient_scene_table = tibble::tibble(),
      response_table = tibble::tibble(),
      situation_table = tibble::tibble(),
      vitals_table = tibble::tibble(),
      disposition_table = tibble::tibble(),
      erecord_01_col = character(),
      epatient_15_col = numeric(),
      epatient_16_col = character(),
      eresponse_05_col = character(),
      esituation_02_col = character(),
      evitals_01_col = date(),
      evitals_27_initial_col = numeric(),
      evitals_27_last_col = numeric(),
      evitals_27_col = NULL,
      edisposition_28_col = character(),
      transport_disposition_col = character()
    )
  )

  testthat::expect_error(
    trauma_03_population(
      patient_scene_table = tibble::tibble(),
      response_table = tibble::tibble(),
      situation_table = tibble::tibble(),
      vitals_table = tibble::tibble(),
      disposition_table = tibble::tibble(),
      erecord_01_col = character(),
      epatient_15_col = numeric(),
      epatient_16_col = character(),
      incident_date_col = character(),
      patient_DOB_col = character(),
      eresponse_05_col = character(),
      esituation_02_col = character(),
      evitals_01_col = date(),
      evitals_27_initial_col = NULL,
      evitals_27_last_col = NULL,
      evitals_27_col = numeric(),
      edisposition_28_col = character(),
      transport_disposition_col = character()
    )
  )

  testthat::expect_error(
    trauma_03_population(
      patient_scene_table = tibble::tibble(),
      response_table = tibble::tibble(),
      situation_table = tibble::tibble(),
      vitals_table = tibble::tibble(),
      disposition_table = tibble::tibble(),
      erecord_01_col = character(),
      epatient_15_col = numeric(),
      epatient_16_col = character(),
      incident_date_col = character(),
      patient_DOB_col = character(),
      eresponse_05_col = character(),
      esituation_02_col = character(),
      evitals_01_col = character(),
      evitals_27_initial_col = NULL,
      evitals_27_last_col = NULL,
      evitals_27_col = numeric(),
      edisposition_28_col = character(),
      transport_disposition_col = character()
    )
  )

  testthat::expect_error(
    trauma_03_population(
      df = tibble::tibble(),
      erecord_01_col = character(),
      epatient_15_col = numeric(),
      epatient_16_col = character(),
      incident_date_col = character(),
      patient_DOB_col = character(),
      eresponse_05_col = character(),
      esituation_02_col = character(),
      evitals_01_col = character(),
      evitals_27_initial_col = NULL,
      evitals_27_last_col = NULL,
      evitals_27_col = numeric(),
      edisposition_28_col = character(),
      transport_disposition_col = character()
    )
  )

})

testthat::test_that("trauma_03_population fails with unknown columns", {

  # Synthetic test data 1
  # for testing a first and last pain scale column
  test_data1 <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    esituation_02 = rep("Yes", 5),
    evitals_01 = lubridate::as_datetime(c("2025-01-01 12:00:00", "2025-01-05 18:00:00", "2025-02-01 06:00:00", "2025-01-01 01:00:00", "2025-06-01 14:00:00")),
    evitals_27_1 = c(0, 2, 4, 6, 8),
    evitals_27_2 = c(0, 1, 3, 5, 7),
    edisposition_28 = rep(4228001, 5),
    edisposition_30 = c(4230001, 4230003, 4230001, 4230007, 4230007)
  )

  # Expand data so each erecord_01 has 3 corresponding evitals_01 timestamps
  test_data_expanded1 <- test_data1 |>
    tidyr::uncount(weights = 3) |>  # Repeat each row 3 times
    dplyr::group_by(erecord_01) |>
    dplyr::mutate(
      time_offset = dplyr::case_when(
        dplyr::row_number() == 1 ~ -5,   # 5 minutes earlier
        dplyr::row_number() == 2 ~ 0,    # Original time
        dplyr::row_number() == 3 ~ 5     # 5 minutes later
      ),
      evitals_01 = evitals_01 + lubridate::dminutes(time_offset)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-time_offset)  # Remove temporary column

  testthat::expect_error(trauma_03_population(test_data_expanded1,
                                              erecord_01_col = erecord_01,
                                              epatient_15_col = epatient_15,
                                              epatient_16_col = epatient_16,
                                              incident_date_col = NULL,
                                              patient_DOB_col = NULL,
                                              eresponse_05_col = eresponse_05,
                                              esituation_02_col = esituation_02,
                                              evitals_01_col = evitals_01,
                                              evitals_27_initial_col = evitals_27_1,
                                              evitals_27_last_col = evitals_27_2,
                                              evitals_27_col = NULL,
                                              edisposition_28_col = "dummy",
                                              transport_disposition_col = "dummy"
  ),
  "exist"
  )

})


testthat::test_that("trauma_03_population correctly classifies patient age", {

  # Synthetic test data 1
  # for testing a first and last pain scale column
  test_data1 <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01", "2025-01-01", "2025-06-01")),
    patient_dob = as.Date(c("2000-01-01", "2020-01-01", "2023-02-01", "2023-01-01", "1970-06-01")),
    epatient_15 = c(25, 5, 2, 2, 55),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Years", "Years"),
    eresponse_05 = rep(2205001, 5),
    esituation_02 = rep("Yes", 5),
    evitals_01 = lubridate::as_datetime(c("2025-01-01 12:00:00", "2025-01-05 18:00:00", "2025-02-01 06:00:00", "2025-01-01 01:00:00", "2025-06-01 14:00:00")),
    evitals_27_1 = c(0, 2, 4, 6, 8),
    evitals_27_2 = c(0, 1, 3, 5, 7),
    edisposition_28 = rep(4228001, 5),
    edisposition_30 = c(4230001, 4230003, 4230001, 4230007, 4230007)
  )

  # Expand data so each erecord_01 has 3 corresponding evitals_01 timestamps
  test_data_expanded1 <- test_data1 |>
    tidyr::uncount(weights = 3) |>  # Repeat each row 3 times
    dplyr::group_by(erecord_01) |>
    dplyr::mutate(
      time_offset = dplyr::case_when(
        dplyr::row_number() == 1 ~ -5,   # 5 minutes earlier
        dplyr::row_number() == 2 ~ 0,    # Original time
        dplyr::row_number() == 3 ~ 5     # 5 minutes later
      ),
      evitals_01 = evitals_01 + lubridate::dminutes(time_offset)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-time_offset)  # Remove temporary column

  # Synthetic test data 2
  # for testing a single pain scale column
  test_data2 <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01", "2025-01-01", "2025-06-01")),
    patient_dob = as.Date(c("2000-01-01", "2020-01-01", "2023-02-01", "2023-01-01", "1970-06-01")),
    epatient_15 = c(25, 5, 2, 2, 55),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Years", "Years"),
    eresponse_05 = rep(2205001, 5),
    esituation_02 = rep("Yes", 5),
    evitals_01 = lubridate::as_datetime(c("2025-01-01 12:00:00", "2025-01-05 18:00:00", "2025-02-01 06:00:00", "2025-01-01 01:00:00", "2025-06-01 14:00:00")),
    edisposition_28 = rep(4228001, 5),
    edisposition_30 = c(4230001, 4230003, 4230001, 4230007, 4230007)
  )

  # Expand data so each erecord_01 has 2 rows (one for each pain score)
  test_data_expanded2 <- test_data2 |>
    tidyr::uncount(weights = 2) |>  # Duplicate each row twice
    dplyr::mutate(evitals_27 = c(0, 0, 2, 1, 4, 3, 6, 5, 8, 7)) |> # Assign pain scores
    dplyr::group_by(erecord_01) |>
    dplyr::mutate(
      time_offset = dplyr::if_else(dplyr::row_number() == 1, -5, 0),  # Lower score = later time
      evitals_01 = evitals_01 + lubridate::dminutes(time_offset)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-time_offset)  # Remove temporary column

  result <- trauma_03_population(test_data_expanded1,
                                 erecord_01_col = erecord_01,
                                 epatient_15_col = epatient_15,
                                 epatient_16_col = epatient_16,
                                 incident_date_col = incident_date,
                                 patient_DOB_col = patient_dob,
                                 eresponse_05_col = eresponse_05,
                                 esituation_02_col = esituation_02,
                                 evitals_01_col = evitals_01,
                                 evitals_27_initial_col = evitals_27_1,
                                 evitals_27_last_col = evitals_27_2,
                                 evitals_27_col = NULL,
                                 edisposition_28_col = edisposition_28,
                                 transport_disposition_col = edisposition_30
                                 )

  testthat::expect_true(all(result$adults$system_age_adult == TRUE))
  testthat::expect_true(all(result$adults$system_age_minor == FALSE))

  result <- trauma_03_population(test_data_expanded2,
                                 erecord_01_col = erecord_01,
                                 epatient_15_col = epatient_15,
                                 epatient_16_col = epatient_16,
                                 incident_date_col = incident_date,
                                 patient_DOB_col = patient_dob,
                                 eresponse_05_col = eresponse_05,
                                 esituation_02_col = esituation_02,
                                 evitals_01_col = evitals_01,
                                 evitals_27_initial_col = NULL,
                                 evitals_27_last_col = NULL,
                                 evitals_27_col = evitals_27,
                                 edisposition_28_col = edisposition_28,
                                 transport_disposition_col = edisposition_30
  )

  testthat::expect_true(all(result$adults$system_age_adult == TRUE))
  testthat::expect_true(all(result$adults$system_age_minor == FALSE))

  # create tables to test correct functioning
  patient_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01", "2025-01-01", "2025-06-01")),
    patient_dob = as.Date(c("2000-01-01", "2020-01-01", "2023-02-01", "2023-01-01", "1970-06-01")),
    epatient_15 = c(25, 5, 2, 2, 55),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Years", "Years")

  )

  # response table
  response_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    eresponse_05 = rep(2205001, 5)

  )

  # situation table
  situation_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    esituation_02 = rep("Yes", 5),
  )

  # vitals table for first and last pain scale columns
  vitals_table_1 <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    evitals_01 = lubridate::as_datetime(c("2025-01-01 12:00:00", "2025-01-05 18:00:00", "2025-02-01 06:00:00", "2025-01-01 01:00:00", "2025-06-01 14:00:00")),
    evitals_27_1 = c(0, 2, 4, 6, 8),
    evitals_27_2 = c(0, 1, 3, 5, 7)
  )  |>
    tidyr::uncount(weights = 3) |>  # Repeat each row 3 times
    dplyr::group_by(erecord_01) |>
    dplyr::mutate(
      time_offset = dplyr::case_when(
        dplyr::row_number() == 1 ~ -5,   # 5 minutes earlier
        dplyr::row_number() == 2 ~ 0,    # Original time
        dplyr::row_number() == 3 ~ 5     # 5 minutes later
      ),
      evitals_01 = evitals_01 + lubridate::dminutes(time_offset)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-time_offset)  # Remove temporary column

  # vitals table for a single pain scale column
  vitals_table_2 <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    evitals_01 = lubridate::as_datetime(c("2025-01-01 12:00:00", "2025-01-05 18:00:00", "2025-02-01 06:00:00", "2025-01-01 01:00:00", "2025-06-01 14:00:00"))
  ) |>
    tidyr::uncount(weights = 2) |>  # Duplicate each row twice
    dplyr::mutate(evitals_27 = c(0, 0, 2, 1, 4, 3, 6, 5, 8, 7)) |> # Assign pain scores
    dplyr::group_by(erecord_01) |>
    dplyr::mutate(
      time_offset = dplyr::if_else(dplyr::row_number() == 1, -5, 0),  # Lower score = later time
      evitals_01 = evitals_01 + lubridate::dminutes(time_offset)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-time_offset)  # Remove temporary column

  disposition_table <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    edisposition_28 = rep(4228001, 5),
    edisposition_30 = c(4230001, 4230003, 4230001, 4230007, 4230007)
  )

  result <- trauma_03_population(patient_scene_table = patient_table,
                                 response_table = response_table,
                                 situation_table = situation_table,
                                 vitals_table = vitals_table_1,
                                 disposition_table = disposition_table,
                                 erecord_01_col = erecord_01,
                                 epatient_15_col = epatient_15,
                                 epatient_16_col = epatient_16,
                                 incident_date_col = incident_date,
                                 patient_DOB_col = patient_dob,
                                 eresponse_05_col = eresponse_05,
                                 esituation_02_col = esituation_02,
                                 evitals_01_col = evitals_01,
                                 evitals_27_initial_col = evitals_27_1,
                                 evitals_27_last_col = evitals_27_2,
                                 evitals_27_col = NULL,
                                 edisposition_28_col = edisposition_28,
                                 transport_disposition_col = edisposition_30
  )

  testthat::expect_true(all(result$adults$system_age_adult == TRUE))
  testthat::expect_true(all(result$adults$system_age_minor == FALSE))

})

testthat::test_that("trauma_03_population correctly filters 911 calls", {

  # Synthetic test data 1
  # for testing a first and last pain scale column
  test_data1 <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    esituation_02 = rep("Yes", 5),
    evitals_01 = lubridate::as_datetime(c("2025-01-01 12:00:00", "2025-01-05 18:00:00", "2025-02-01 06:00:00", "2025-01-01 01:00:00", "2025-06-01 14:00:00")),
    evitals_27_1 = c(0, 2, 4, 6, 8),
    evitals_27_2 = c(0, 1, 3, 5, 7),
    edisposition_28 = rep(4228001, 5),
    edisposition_30 = c(4230001, 4230003, 4230001, 4230007, 4230007)
  )

  # Expand data so each erecord_01 has 3 corresponding evitals_01 timestamps
  test_data_expanded1 <- test_data1 |>
    tidyr::uncount(weights = 3) |>  # Repeat each row 3 times
    dplyr::group_by(erecord_01) |>
    dplyr::mutate(
      time_offset = dplyr::case_when(
        dplyr::row_number() == 1 ~ -5,   # 5 minutes earlier
        dplyr::row_number() == 2 ~ 0,    # Original time
        dplyr::row_number() == 3 ~ 5     # 5 minutes later
      ),
      evitals_01 = evitals_01 + lubridate::dminutes(time_offset)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-time_offset)  # Remove temporary column

  result <- trauma_03_population(test_data_expanded1,
                                 erecord_01_col = erecord_01,
                                 epatient_15_col = epatient_15,
                                 epatient_16_col = epatient_16,
                                 incident_date_col = NULL,
                                 patient_DOB_col = NULL,
                                 eresponse_05_col = eresponse_05,
                                 esituation_02_col = esituation_02,
                                 evitals_01_col = evitals_01,
                                 evitals_27_initial_col = evitals_27_1,
                                 evitals_27_last_col = evitals_27_2,
                                 evitals_27_col = NULL,
                                 edisposition_28_col = edisposition_28,
                                 transport_disposition_col = edisposition_30
                                 )

  emergency_calls <- result$filter_process |>
    dplyr::filter(filter == "911 calls") |>
    dplyr::pull(count)

  testthat::expect_equal(nrow(result$filter_process), 10)
  testthat::expect_equal(emergency_calls, 5)

})

testthat::test_that("trauma_03_population runs correctly with table inputs", {

  # create tables to test correct functioning
  patient_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01", "2025-01-01", "2025-06-01")),
    patient_dob = as.Date(c("2000-01-01", "2020-01-01", "2023-02-01", "2023-01-01", "1970-06-01")),
    epatient_15 = c(25, 5, 2, 2, 55),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Years", "Years")

  )

  # response table
  response_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    eresponse_05 = rep(2205001, 5)

  )

  # situation table
  situation_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    esituation_02 = rep("Yes", 5),
  )

  # vitals table for first and last pain scale columns
  vitals_table_1 <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    evitals_01 = lubridate::as_datetime(c("2025-01-01 12:00:00", "2025-01-05 18:00:00", "2025-02-01 06:00:00", "2025-01-01 01:00:00", "2025-06-01 14:00:00")),
    evitals_27_1 = c(0, 2, 4, 6, 8),
    evitals_27_2 = c(0, 1, 3, 5, 7)
  )  |>
    tidyr::uncount(weights = 3) |>  # Repeat each row 3 times
    dplyr::group_by(erecord_01) |>
    dplyr::mutate(
      time_offset = dplyr::case_when(
        dplyr::row_number() == 1 ~ -5,   # 5 minutes earlier
        dplyr::row_number() == 2 ~ 0,    # Original time
        dplyr::row_number() == 3 ~ 5     # 5 minutes later
      ),
      evitals_01 = evitals_01 + lubridate::dminutes(time_offset)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-time_offset)  # Remove temporary column

  # vitals table for a single pain scale column
  vitals_table_2 <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    evitals_01 = lubridate::as_datetime(c("2025-01-01 12:00:00", "2025-01-05 18:00:00", "2025-02-01 06:00:00", "2025-01-01 01:00:00", "2025-06-01 14:00:00"))
  ) |>
    tidyr::uncount(weights = 2) |>  # Duplicate each row twice
    dplyr::mutate(evitals_27 = c(0, 0, 2, 1, 4, 3, 6, 5, 8, 7)) |> # Assign pain scores
    dplyr::group_by(erecord_01) |>
    dplyr::mutate(
      time_offset = dplyr::if_else(dplyr::row_number() == 1, -5, 0),  # Lower score = later time
      evitals_01 = evitals_01 + lubridate::dminutes(time_offset)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-time_offset)  # Remove temporary column

  disposition_table <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    edisposition_28 = rep(4228001, 5),
    edisposition_30 = c(4230001, 4230003, 4230001, 4230007, 4230007)
  )

  # test the success of the function

  result <- trauma_03_population(patient_scene_table = patient_table,
                                 response_table = response_table,
                                 situation_table = situation_table,
                                 vitals_table = vitals_table_1,
                                 disposition_table = disposition_table,
                                 erecord_01_col = erecord_01,
                                 epatient_15_col = epatient_15,
                                 epatient_16_col = epatient_16,
                                 incident_date_col = NULL,
                                 patient_DOB_col = NULL,
                                 eresponse_05_col = eresponse_05,
                                 esituation_02_col = esituation_02,
                                 evitals_01_col = evitals_01,
                                 evitals_27_initial_col = evitals_27_1,
                                 evitals_27_last_col = evitals_27_2,
                                 evitals_27_col = NULL,
                                 edisposition_28_col = edisposition_28,
                                 transport_disposition_col = edisposition_30
                                 )

  testthat::expect_equal(nrow(result$filter_process), 10)
  testthat::expect_true(is.list(result))

  result <- trauma_03_population(patient_scene_table = patient_table,
                                 response_table = response_table,
                                 situation_table = situation_table,
                                 vitals_table = vitals_table_2,
                                 disposition_table = disposition_table,
                                 erecord_01_col = erecord_01,
                                 epatient_15_col = epatient_15,
                                 epatient_16_col = epatient_16,
                                 incident_date_col = NULL,
                                 patient_DOB_col = NULL,
                                 eresponse_05_col = eresponse_05,
                                 esituation_02_col = esituation_02,
                                 evitals_01_col = evitals_01,
                                 evitals_27_initial_col = NULL,
                                 evitals_27_last_col = NULL,
                                 evitals_27_col = evitals_27,
                                 edisposition_28_col = edisposition_28,
                                 transport_disposition_col = edisposition_30
                                 )

  testthat::expect_equal(nrow(result$filter_process), 10)
  testthat::expect_true(is.list(result))

})
