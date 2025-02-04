#' Synthetic eProcedures Data from the National Emergency Medical Services
#' Information System (NEMSIS)
#'
#' This dataset provides synthetic procedure-related information from the
#' eProcedures section of the National Emergency Medical Services Information
#' System (NEMSIS). It contains example procedure details that can be used for
#' testing various functions within the `nemsqar` package. Users are encouraged
#' to test these functions with this dataset, but results should not be
#' interpreted as meaningful. Some outputs may be nonsensical, which is expected
#' since this data is only intended to demonstrate the expected structure of
#' input data.
#'
#' @format A tibble with 10,000 rows and 8 variables:
#' \describe{
#'   \item{`Incident Patient Care Report Number - PCR (eRecord.01)`}{(chr) Unique identifier for the patient care report.}
#'   \item{`Incident Date`}{(date) The date of the EMS incident.}
#'   \item{`Procedure Performed Date Time (eProcedures.01)`}{(dttm) The date and time the procedure was performed.}
#'   \item{`Procedure Performed Prior To EMS Care (eProcedures.02)`}{(chr) Indicates whether the procedure was performed before EMS arrival.}
#'   \item{`Procedure Performed Description And Code (eProcedures.03)`}{(chr) Description and code of the performed procedure.}
#'   \item{`Patient Attempted Procedure Descriptions And Codes List (eProcedures.03)`}{(chr) List of attempted procedures with descriptions and codes.}
#'   \item{`Procedure Number Of Attempts (eProcedures.05)`}{(dbl) Number of attempts made to perform the procedure.}
#'   \item{`Procedure Successful (eProcedures.06)`}{(chr) Indicates whether the procedure was successful.}
#' }
#'
#' @details The data in this table are entirely synthetic and intended solely
#' for testing purposes. These data do not represent real patients, incidents,
#' or outcomes and should not be used for research or operational
#' decision-making.
#'
#' @examples
#' data(nemsqar_procedures_table)
#' dplyr::glimpse(nemsqar_procedures_table)
#'
"nemsqar_procedures_table"
