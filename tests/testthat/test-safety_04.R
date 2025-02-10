testthat::test_that("safety_04 produces expected results", {

  # Synthetic test data
  test_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    earrest_01 = rep("No", 5),
    einjury_03 = rep("non-injury", 5),
    edisposition_14 = rep(4214001, 5),
    edisposition_30 = rep(4230001, 5),
    eprocedures_03 = rep("other response", 5)
  )

  # Run function
  result <- safety_04(
    df = test_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    earrest_01_col = earrest_01,
    einjury_03_col = einjury_03,
    edisposition_14_col = edisposition_14,
    transport_disposition_col = edisposition_30,
    eprocedures_03_col = eprocedures_03
  )

  # Check structure
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(all(c("measure", "pop", "numerator", "denominator", "prop", "prop_label") %in% names(result)))

  # Check calculations
  testthat::expect_equal(sum(result$numerator), 2)  # Three cases had Albuterol
  testthat::expect_equal(sum(result$denominator), 2)  # Four cases met inclusion criteria
  testthat::expect_equal(result$prop[result$pop == "Peds"], 1)
  testthat::expect_equal(nrow(result), 1)

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

  disposition_table <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    edisposition_14 = rep(4214001, 5),
    edisposition_30 = rep(4230001, 5),
  )

  arrest_table <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    earrest_01 = rep("No", 5)
  )

  injury_table <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    einjury_03 = rep("non-injury", 5)
  )

  procedures_table <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    eprocedures_03 = rep("other response", 5)
  )

  # test the success of the function

  result_2 <- safety_04(patient_scene_table = patient_table,
                        response_table = response_table,
                        arrest_table = arrest_table,
                        injury_table = injury_table,
                        procedures_table = procedures_table,
                        disposition_table = disposition_table,
                        erecord_01_col = erecord_01,
                        epatient_15_col = epatient_15,
                        epatient_16_col = epatient_16,
                        eresponse_05_col = eresponse_05,
                        earrest_01_col = earrest_01,
                        einjury_03_col = einjury_03,
                        edisposition_14_col = edisposition_14,
                        transport_disposition_col = edisposition_30,
                        eprocedures_03_col = eprocedures_03
                        )

  # Check calculations
  testthat::expect_equal(sum(result_2$numerator), 3)  # Three cases had Albuterol
  testthat::expect_equal(sum(result_2$denominator), 3)  # Four cases met inclusion criteria
  testthat::expect_equal(result_2$prop[result_2$pop == "Peds"], 1)
  testthat::expect_equal(nrow(result_2), 1)


})

testthat::test_that("safety_04 handles missing data correctly", {

  # Synthetic test data
  missing_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(NA_character_, 5),
    earrest_01 = rep("No", 5),
    einjury_03 = rep("non-injury", 5),
    edisposition_14 = rep(4214001, 5),
    edisposition_30 = rep(4230001, 5),
    eprocedures_03 = rep("other response", 5)
  )

  result <- safety_04(
    df = missing_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    earrest_01_col = earrest_01,
    einjury_03_col = einjury_03,
    edisposition_14_col = edisposition_14,
    transport_disposition_col = edisposition_30,
    eprocedures_03_col = eprocedures_03
  )

  testthat::expect_true(nrow(result) > 0)
  testthat::expect_true(all(!is.na(result$denominator)))

})

testthat::test_that("safety_04 returns empty result for non-matching criteria", {

  # Synthetic test data
  non_matching_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    earrest_01 = rep("No", 5),
    einjury_03 = rep("non-injury", 5),
    edisposition_14 = rep("chair", 5),
    edisposition_30 = rep("private car", 5),
    eprocedures_03 = rep("other response", 5)
  )

  result <- safety_04(
    df = non_matching_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    earrest_01_col = earrest_01,
    einjury_03_col = einjury_03,
    edisposition_14_col = edisposition_14,
    transport_disposition_col = edisposition_30,
    eprocedures_03_col = eprocedures_03
  )

  testthat::expect_equal(sum(result$denominator), 0)
})

