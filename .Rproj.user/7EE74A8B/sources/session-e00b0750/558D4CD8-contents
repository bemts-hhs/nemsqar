testthat::test_that("trauma_03 produces expected results", {

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

  # Synthetic test data 2
  # for testing a single pain scale column
  test_data2 <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
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

  # Run function
  result <- suppressWarnings(trauma_03(
    df = test_data_expanded1,
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
    transport_disposition_col = edisposition_30,
    confidence_interval = TRUE
  ))

  # Check structure
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(all(c("measure", "pop", "numerator", "denominator", "prop", "prop_label", "lower_ci", "upper_ci") %in% names(result)))

  # should throw a warning due to small counts
  testthat::expect_warning(trauma_03(
    df = test_data_expanded1,
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
    transport_disposition_col = edisposition_30,
    confidence_interval = TRUE
  ))

  # Run function with the first and last pain score columns
  result_1 <- trauma_03(
    df = test_data_expanded1,
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

  # Check structure
  testthat::expect_s3_class(result_1, "data.frame")
  testthat::expect_true(all(c("measure", "pop", "numerator", "denominator", "prop", "prop_label") %in% names(result_1)))

  # Check calculations
  testthat::expect_equal(sum(result_1$numerator), 7)
  testthat::expect_equal(sum(result_1$denominator), 7)
  testthat::expect_equal(result_1$prop[result_1$pop == "Adults"], 1)
  testthat::expect_equal(nrow(result_1), 3)

  # Run function with the single pain score column
  result_2 <- trauma_03(
    df = test_data_expanded2,
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

  # Check structure
  testthat::expect_s3_class(result_2, "data.frame")
  testthat::expect_true(all(c("measure", "pop", "numerator", "denominator", "prop", "prop_label") %in% names(result_2)))

  # Check calculations
  testthat::expect_equal(sum(result_2$numerator), 7)
  testthat::expect_equal(sum(result_2$denominator), 7)
  testthat::expect_equal(result_2$prop[result_2$pop == "Adults"], 1)
  testthat::expect_equal(nrow(result_2), 3)

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
  # use the initial and last pain scale columns
  result_3 <- trauma_03(patient_scene_table = patient_table,
                        response_table = response_table,
                        situation_table = situation_table,
                        vitals_table = vitals_table_1,
                        disposition_table = disposition_table,
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

  # Check calculations
  testthat::expect_equal(sum(result_3$numerator), 8)
  testthat::expect_equal(sum(result_3$denominator), 8)
  testthat::expect_equal(result_3$prop[result_3$pop == "Adults"], 1)
  testthat::expect_equal(nrow(result_3), 3)

  # test the success of the function
  # use the single pain scale column
  result_4 <- trauma_03(patient_scene_table = patient_table,
                        response_table = response_table,
                        situation_table = situation_table,
                        vitals_table = vitals_table_2,
                        disposition_table = disposition_table,
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

  # Check calculations
  testthat::expect_equal(sum(result_4$numerator), 8)
  testthat::expect_equal(sum(result_4$denominator), 8)
  testthat::expect_equal(result_4$prop[result_4$pop == "Adults"], 1)
  testthat::expect_equal(nrow(result_4), 3)


})

testthat::test_that("trauma_03 handles missing data correctly", {

  # Synthetic test data 1
  # for testing a first and last pain scale column
  test_data1 <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    esituation_02 = rep("Yes", 5),
    evitals_01 = lubridate::as_datetime(c(NA, "2025-01-05 18:00:00", "2025-02-01 06:00:00", "2025-01-01 01:00:00", "2025-06-01 14:00:00")),
    evitals_27_1 = c(0, 2, 4, 6, NA),
    evitals_27_2 = c(0, 1, 3, 5, NA),
    edisposition_28 = rep(4228001, 5),
    edisposition_30 = c(4230001, NA, 4230001, 4230007, NA)
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
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    esituation_02 = rep("Yes", 5),
    evitals_01 = lubridate::as_datetime(c(NA, "2025-01-05 18:00:00", "2025-02-01 06:00:00", "2025-01-01 01:00:00", "2025-06-01 14:00:00")),
    edisposition_28 = rep(4228001, 5),
    edisposition_30 = c(4230001, NA, 4230001, 4230007, NA)
  )

  # Expand data so each erecord_01 has 2 rows (one for each pain score)
  test_data_expanded2 <- test_data2 |>
    tidyr::uncount(weights = 2) |>  # Duplicate each row twice
    dplyr::mutate(evitals_27 = c(0, 0, 2, NA, 4, 3, 6, 5, 8, NA)) |> # Assign pain scores
    dplyr::group_by(erecord_01) |>
    dplyr::mutate(
      time_offset = dplyr::if_else(dplyr::row_number() == 1, -5, 0),  # Lower score = later time
      evitals_01 = evitals_01 + lubridate::dminutes(time_offset)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-time_offset)  # Remove temporary column

  # run the function with first and last
  # pain scale columns
  result_1 <- trauma_03(
    df = test_data_expanded1,
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

  testthat::expect_true(nrow(result_1) > 0)
  testthat::expect_true(all(!is.na(result_1$denominator)))

  # run the function with the single pain scale
  # column
  result_2 <- trauma_03(
    df = test_data_expanded2,
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

  testthat::expect_true(nrow(result_2) > 0)
  testthat::expect_true(all(!is.na(result_2$denominator)))

})

testthat::test_that("trauma_03 returns empty result for non-matching criteria", {

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
    edisposition_30 = rep("not a transport", 5)
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
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    esituation_02 = rep("Yes", 5),
    evitals_01 = lubridate::as_datetime(c("2025-01-01 12:00:00", "2025-01-05 18:00:00", "2025-02-01 06:00:00", "2025-01-01 01:00:00", "2025-06-01 14:00:00")),
    edisposition_28 = rep(4228001, 5),
    edisposition_30 = rep("not a transport", 5)
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

  # run the function with the
  # initial and last pain scale
  # columns
  result_1 <- trauma_03(
    df = test_data_expanded1,
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

  testthat::expect_equal(sum(result_1$denominator), 0)

  # run the function with the
  # single pain scale
  # column
  result_2 <- trauma_03(
    df = test_data_expanded2,
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

  testthat::expect_equal(sum(result_2$denominator), 0)

})

