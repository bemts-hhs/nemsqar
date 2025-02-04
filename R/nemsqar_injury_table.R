#' Synthetic Test Data for eInjury Fields in National EMS Information System
#'
#' This dataset provides completely synthetic test data for evaluating injury-related
#' functions in the `nemsqar` package. It includes key variables related to the
#' cause of injury, trauma triage criteria, vehicular risk factors, and height of falls.
#' The dataset is intended to assist users in testing the expected input structure
#' for injury-related measures.
#'
#' Users are encouraged to test these functions with this dataset, but results
#' should not be interpreted as meaningful. Some outputs may be nonsensical,
#' which is expected since this data is only intended to demonstrate the
#' expected structure of input data.
#'
#' @format A data frame with 10,000 rows and 8 variables:
#' \describe{
#'   \item{Incident Patient Care Report Number - PCR (eRecord.01)}{Unique identifier for the incident report (character).}
#'   \item{Incident Date}{Date of the incident (Date).}
#'   \item{Injury Cause of Injury (eInjury.01)}{General description of the cause of injury (character).}
#'   \item{Injury Cause Of Injury Description And Code List (eInjury.01)}{Detailed description and coding of injury causes (character).}
#'   \item{Injury Trauma Center/Triage Criteria (Steps 1 and 2) List (eInjury.03)}{List of trauma triage criteria met in Steps 1 and 2 (character).}
#'   \item{Injury Vehicular Pedestrian Or Other Injury Risk Factor/Triage Criteria (Steps 3 and 4) (eInjury.04)}{Primary vehicular or other risk factors for injury (character).}
#'   \item{Injury Vehicular Pedestrian Or Other Injury Risk Factor/Triage Criteria (Steps 3 and 4) List (eInjury.04)}{Detailed list of vehicular or pedestrian injury risk factors (character).}
#'   \item{Injury Height Of Fall In Feet (eInjury.09)}{Height of fall in feet when applicable (numeric).}
#' }
#'
#' @usage data(nemsqar_injury_table)
#'
#' @examples
#' data(nemsqar_injury_table)
#' head(nemsqar_injury_table)
#'
"nemsqar_injury_table"
