#' Synthetic eVitals Data for NEMSIS from the National Emergency Medical Services
#' Information System (NEMSIS)
#'
#' This dataset contains synthetic data for the eVitals section of the
#' National Emergency Medical Services Information System (NEMSIS).
#' It is designed for testing functions within the nemsqar package.
#' The data structure follows the expected format for eVitals fields
#' but does not produce meaningful clinical results.
#'
#' Users are encouraged to use this dataset to test functions, but
#' the outputs may be nonsensical, as the data is solely intended to
#' demonstrate the expected input structure.
#'
#' @format A tibble with 1,000 rows and 19 variables:
#' \describe{
#'   \item{Incident Patient Care Report Number - PCR (eRecord.01)}{Unique incident identifier (character).}
#'   \item{Incident Date}{Date of the EMS incident (Date).}
#'   \item{Vitals Signs Taken Date Time (eVitals.01)}{Timestamp of vital signs measurement (Datetime).}
#'   \item{Vitals ECG Type (eVitals.04)}{ECG type recorded (character).}
#'   \item{Vitals Systolic Blood Pressure SBP (eVitals.06)}{Systolic blood pressure (numeric).}
#'   \item{Vitals Diastolic Blood Pressure DBP (eVitals.07)}{Diastolic blood pressure (numeric).}
#'   \item{Vitals Heart Rate (eVitals.10)}{Heart rate in beats per minute (numeric).}
#'   \item{Vitals Pulse Oximetry (eVitals.12)}{Oxygen saturation percentage (numeric).}
#'   \item{Vitals Respiratory Rate (eVitals.14)}{Respiratory rate in breaths per minute (numeric).}
#'   \item{Vitals Respiratory Effort (eVitals.15)}{Observed respiratory effort (character).}
#'   \item{Vitals Carbon Dioxide CO2 (eVitals.16)}{End-tidal CO2 measurement (numeric).}
#'   \item{Vitals Blood Glucose Level (eVitals.18)}{Blood glucose level in mg/dL (numeric).}
#'   \item{Vitals Glasgow Coma Score GCS Motor (eVitals.21)}{GCS motor response score (character).}
#'   \item{Vitals Total Glasgow Coma Score GCS (eVitals.23)}{Total Glasgow Coma Score (numeric).}
#'   \item{Vitals Level Of Responsiveness AVPU (eVitals.26)}{AVPU scale assessment (character).}
#'   \item{Vitals Pain Scale Score (eVitals.27)}{Pain scale score (numeric).}
#'   \item{Vitals Pain Scale Score Range Sort Order (eVitals.27)}{Sort order for pain scale (numeric).}
#'   \item{Vitals Stroke Scale Score (eVitals.29)}{Stroke scale assessment result (character).}
#'   \item{Vitals Stroke Scale Type (eVitals.30)}{Type of stroke scale used (character).}
#' }
#'
#' @examples
#' data(nemsqar_vitals_table)
#' dplyr::glimpse(nemsqar_vitals_table)
#'
"nemsqar_vitals_table"
