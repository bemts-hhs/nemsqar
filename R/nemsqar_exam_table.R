#' Synthetic Test Data for eExam Fields in National EMS Information System
#'
#' This dataset provides completely synthetic test data for evaluating patient
#' examination-related functions in the `nemsqar` package. It includes key
#' variables related to patient weight, assessment findings, lung and chest
#' examinations, and neurological assessments.
#' The dataset is intended to assist users in testing the expected input structure
#' for examination-related measures.
#'
#' Users are encouraged to test these functions with this dataset, but results
#' should not be interpreted as meaningful. Some outputs may be nonsensical,
#' which is expected since this data is only intended to demonstrate the
#' expected structure of input data.
#'
#' @format A data frame with 10,000 rows and 11 variables:
#' \describe{
#'   \item{Incident Patient Care Report Number - PCR (eRecord.01)}{Unique identifier for the incident report (character).}
#'   \item{Incident Date}{Date of the incident (Date).}
#'   \item{Patient Weight In Kilograms (eExam.01)}{Patient's weight in kilograms (numeric).}
#'   \item{Patient Length Based Color (eExam.02)}{Color coding based on patient length for pediatric patients (character).}
#'   \item{Patient Assessment Date Time (eExam.03)}{Timestamp for patient assessment (datetime).}
#'   \item{Patient Extremity Assessment Findings List (eExam.16)}{List of findings related to extremity assessment (character).}
#'   \item{Patient Neurological Assessment Findings List (eExam.20)}{List of neurological assessment findings (character).}
#'   \item{Patient Lung Assessment Finding Location (3.4=itExam.099/3.5=eExam.22)}{Location of lung assessment findings (character).}
#'   \item{Patient Lung Assessment Findings List (3.4=itExam.100/3.5=eExam.23)}{List of lung assessment findings (character).}
#'   \item{Patient ChestExclusive Assessment Finding Location (3.4=itExam.101/3.5=eExam.24)}{Location of chest-exclusive assessment findings (character).}
#'   \item{Patient ChestExclusive Assessment Findings List (3.4=itExam.102/3.5=eExam.25)}{List of chest-exclusive assessment findings (character).}
#' }
#'
#' @usage data(nemsqar_exam_table)
#'
#' @examples
#' data(nemsqar_exam_table)
#' head(nemsqar_exam_table)
#'
"nemsqar_exam_table"
