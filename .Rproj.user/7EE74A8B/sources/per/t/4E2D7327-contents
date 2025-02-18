testthat::test_that("stroke_01_population rejects invalid argument combinations", {

  testthat::expect_error(stroke_01_population(df = tibble::tibble(),
                                                    patient_scene_table = tibble::tibble(),
                                                    response_table = tibble::tibble(),
                                                    situation_table = tibble::tibble(),
                                                    vitals_table = tibble::tibble(),
                                                    erecord_01_col = character(),
                                                    eresponse_05_col = character(),
                                                    esituation_11_col = character(),
                                                    esituation_12_col = character(),
                                                    evitals_23_col = character(),
                                                    evitals_26_col = numeric(),
                                                    evitals_29_col = numeric(),
                                                    evitals_30_col = character()
                                              ),
                         "requires either a"
                         )

  testthat::expect_error(stroke_01_population(patient_scene_table = list(),
                                              response_table = tibble::tibble(),
                                              situation_table = tibble::tibble(),
                                              vitals_table = tibble::tibble(),
                                              erecord_01_col = character(),
                                              eresponse_05_col = character(),
                                              esituation_11_col = character(),
                                              esituation_12_col = character(),
                                              evitals_23_col = character(),
                                              evitals_26_col = numeric(),
                                              evitals_29_col = numeric(),
                                              evitals_30_col = character()
                                              ),
  "One or more of the tables passed to")
})

testthat::test_that("stroke_01_population rejects missing required column arguments", {
  testthat::expect_error(stroke_01_population(df = tibble::tibble(), evitals_23_col = 12),
                         "One or more of the \\*_col arguments is missing")
})

testthat::test_that("stroke_01_population rejects non-dataframe inputs", {
  testthat::expect_error(stroke_01_population(df = list()),
                         "One or more")

  testthat::expect_error(stroke_01_population(patient_scene_table = matrix()),
                         "One or more")
})

testthat::test_that("stroke_01_population fails with unknown columns", {
  df <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    esituation_11 = c(rep("I60", 3), rep("I61", 2)),
    esituation_12 = c(rep("I63", 2), rep("I64", 3)),
    evitals_23 = c(16, 15, 14, 13, 12),
    evitals_26 = c("Alert", "Painful", "Verbal", "Unresponsive", "Alert"),
    evitals_29 = rep("positive", 5),
    evitals_30 = rep("a pain scale", 5)
  )

  testthat::expect_error(stroke_01_population(df,
                                                    erecord_01_col = erecord_01,
                                                    eresponse_05_col = character(),
                                                    esituation_11_col = "dummy",
                                                    esituation_12_col = "dummy",
                                                    evitals_23_col = "dummy",
                                                    evitals_26_col = "dummy",
                                                    evitals_29_col = "dummy",
                                                    evitals_30_col = "dummy"
                                              ),
                         "exist"
                         )

})


testthat::test_that("stroke_01_population correctly classifies patient age", {
  df <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    esituation_11 = c(rep("I60", 3), rep("I61", 2)),
    esituation_12 = c(rep("I63", 2), rep("I64", 3)),
    evitals_23 = c(16, 15, 14, 13, 12),
    evitals_26 = c("Alert", "Painful", "Verbal", "Unresponsive", "Alert"),
    evitals_29 = rep("positive", 5),
    evitals_30 = rep("a pain scale", 5)
  )

  result <- stroke_01_population(df,
                                       erecord_01_col = erecord_01,
                                       eresponse_05_col = eresponse_05,
                                       esituation_11_col = esituation_11,
                                       esituation_12_col = esituation_12,
                                       evitals_23_col = evitals_23,
                                       evitals_26_col = evitals_26,
                                       evitals_29_col = evitals_29,
                                       evitals_30_col = evitals_30
  )

  testthat::expect_true(all(result$adults$system_age_adult == TRUE))
  testthat::expect_true(all(result$adults$system_age_minor == FALSE))
})

testthat::test_that("stroke_01_population correctly filters 911 calls", {

  df <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    esituation_11 = c(rep("I60", 3), rep("I61", 2)),
    esituation_12 = c(rep("I63", 2), rep("I64", 3)),
    evitals_23 = c(16, 15, 14, 13, 12),
    evitals_26 = c("Alert", "Painful", "Verbal", "Unresponsive", "Alert"),
    evitals_29 = rep("positive", 5),
    evitals_30 = rep("a pain scale", 5)
  )

  result <- stroke_01_population(df,
                                 erecord_01_col = erecord_01,
                                 eresponse_05_col = eresponse_05,
                                 esituation_11_col = esituation_11,
                                 esituation_12_col = esituation_12,
                                 evitals_29_col = evitals_29,
                                 evitals_23_col = evitals_23,
                                 evitals_26_col = evitals_26,
                                 evitals_30_col = evitals_30
  )

  emergency_calls <- result$filter_process |>
    dplyr::filter(filter == "911 calls") |>
    dplyr::pull(count)

  testthat::expect_equal(nrow(result$filter_process), 7)
  testthat::expect_equal(emergency_calls, 5)

})

testthat::test_that("stroke_01_population runs correctly with table inputs", {

  # create tables to test correct functioning
  patient_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    incident_date = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01", "2025-01-01", "2025-06-01")),
    patient_dob = as.Date(c("2000-01-01", "2020-01-01", "2023-02-01", "2023-01-01", "1970-06-01")),
    epatient_15 = c(25, 5, 2, 2, 55),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Years", "Years")

  )

  response_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    eresponse_05 = rep(2205001, 5)

  )

  situation_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    esituation_11 = c(rep("I60", 3), rep("I61", 2)),
    esituation_12 = c(rep("I63", 2), rep("I64", 3)),
  )

  vitals_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    evitals_23 = c(16, 15, 14, 13, 12),
    evitals_26 = c("Alert", "Painful", "Verbal", "Unresponsive", "Alert"),
    evitals_29 = rep("positive", 5),
    evitals_30 = rep("a pain scale", 5)
  )

  # test the success of the function

  result <- stroke_01_population(patient_scene_table = patient_table,
                                 response_table = response_table,
                                 situation_table = situation_table,
                                 vitals_table = vitals_table,
                                 erecord_01_col = erecord_01,
                                 eresponse_05_col = eresponse_05,
                                 esituation_11_col = esituation_11,
                                 esituation_12_col = esituation_12,
                                 evitals_29_col = evitals_29,
                                 evitals_23_col = evitals_23,
                                 evitals_26_col = evitals_26,
                                 evitals_30_col = evitals_30
  )

  testthat::expect_equal(nrow(result$filter_process), 7)
  testthat::expect_true(is.list(result))

})
