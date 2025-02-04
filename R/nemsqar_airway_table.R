#' Synthetic Test Data for eAirway Fields in National EMS Information System
#'
#' This dataset provides completely synthetic test data for the airway-related fields
#' in the National EMS Information System (NEMSIS). It is not specific to any single function
#' but can be used to test multiple functions in the `nemsqar` package. Users are encouraged to
#' experiment with this dataset, but results should not be interpreted as meaningful. Some
#' outputs may be nonsensical, which is expected since this data is only intended to demonstrate
#' the expected structure of input data.
#'
#' @format A data frame with 10,000 rows and 8 variables:
#' \describe{
#'   \item{Incident Patient Care Report Number - PCR (eRecord.01)}{Unique identifier for the incident report (character).}
#'   \item{Incident Date}{Date of the incident (Date).}
#'   \item{Airway Indications For Invasive Management List (eAirway.01)}{List of indications for invasive airway management (character).}
#'   \item{Airway Device Placement Confirmation Date Time (eAirway.02)}{Timestamp of airway device placement confirmation (datetime).}
#'   \item{Airway Device Being Confirmed (eAirway.03)}{Type of airway device being confirmed (character).}
#'   \item{Patient Airway Device Being Confirmed List (eAirway.03)}{List of airway devices being confirmed (character).}
#'   \item{Airway Device Placement Confirmed Method (eAirway.04)}{Primary method used to confirm airway device placement (character).}
#'   \item{Airway Device Placement Confirmed Method List (eAirway.04)}{List of methods used to confirm airway device placement (character).}
#' }
#'
#' @usage data(nemsqar_airway_table)
#'
#' @examples
#' data(nemsqar_airway_table)
#' head(nemsqar_airway_table)
#'
"nemsqar_airway_table"
