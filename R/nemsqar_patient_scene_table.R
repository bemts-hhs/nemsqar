#' Synthetic ePatient Data from the National Emergency Medical Services
#' Information System (NEMSIS)
#'
#' This dataset provides synthetic patient information from the
#' ePatient section of the National Emergency Medical Services Information
#' System (NEMSIS). It contains example patient demographics and scene-related
#' details that can be used for testing various functions within the `nemsqar`
#' package. Users are encouraged to test these functions with this dataset, but
#' results should not be interpreted as meaningful. Some outputs may be
#' nonsensical, which is expected since this data is only intended to
#' demonstrate the expected structure of input data.
#'
#' @format A tibble with 10,000 rows and 6 variables:
#' \describe{
#'   \item{`Incident Patient Care Report Number - PCR (eRecord.01)`}{(chr) Unique identifier for the patient care report.}
#'   \item{`Incident Date`}{(date) The date of the EMS incident.}
#'   \item{`Patient Age (ePatient.15)`}{(dbl) Patient's age at the time of the incident.}
#'   \item{`Patient Age Units (ePatient.16)`}{(chr) The units of measurement for patient age (e.g., Years, Months, Hours, etc.).}
#'   \item{`Patient Date Of Birth (ePatient.17)`}{(date) The patient's date of birth.}
#'   \item{`Patient Gender (ePatient.13)`}{(chr) The patient's gender as recorded in the EMS report.}
#' }
#'
#' @details The data in this table are entirely synthetic and intended solely
#'   for testing purposes. These data do not represent real patients, incidents,
#'   or outcomes and should not be used for research or operational
#'   decision-making.
#'
#' @examples
#' data(nemsqar_patient_scene_table)
#' dplyr::glimpse(nemsqar_patient_scene_table)
#'
"nemsqar_patient_scene_table"
