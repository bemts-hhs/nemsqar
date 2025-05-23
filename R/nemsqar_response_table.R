#' Synthetic eResponse Data from the National Emergency Medical Services
#' Information System (NEMSIS)
#'
#' A synthetic dataset representing the eResponse section of the National
#' Emergency Medical Services Information System (NEMSIS). This dataset is
#' intended for testing purposes only and does not contain real patient or
#' incident data. Users are encouraged to test functions with this dataset, but
#' results should not be interpreted as meaningful. Some outputs may be
#' nonsensical, which is expected since this data is only intended to
#' demonstrate the expected structure of input data.
#'
#' @format A tibble with 10,000 rows and 5 variables:
#' \describe{
#'   \item{Incident Patient Care Report Number - PCR (eRecord.01)}{Character. Unique identifier for the patient care report.}
#'   \item{Incident Date}{Date. The date of the EMS incident.}
#'   \item{Response Type Of Service Requested With Code (eResponse.05)}{Character. The type of service requested, including a coded value.}
#'   \item{Response Type Of Scene Delay List (eResponse.10)}{Character. A list of delay types encountered at the scene.}
#'   \item{Response Additional Response Mode Descriptors List (eResponse.24)}{Character. A list of additional response mode descriptors.}
#' }
#'
#' @usage data(nemsqar_response_table)
#' @keywords datasets
"nemsqar_response_table"
