#' Synthetic Test Data for eArrest Fields in National EMS Information System
#'
#' This dataset provides completely synthetic test data for the cardiac arrest-related fields
#' in the National EMS Information System (NEMSIS). It is not specific to any single function
#' but can be used to test multiple functions in the `nemsqar` package. Users are encouraged to
#' experiment with this dataset, but results should not be interpreted as meaningful. Some
#' outputs may be nonsensical, which is expected since this data is only intended to demonstrate
#' the expected structure of input data.
#'
#' @format A data frame with 10,000 rows and 28 variables:
#' \describe{
#'   \item{Incident Patient Care Report Number - PCR (eRecord.01)}{Unique identifier for the incident report (character).}
#'   \item{Incident Date}{Date of the incident (Date).}
#'   \item{Cardiac Arrest During EMS Event With Code (eArrest.01)}{Indicates whether cardiac arrest occurred during the EMS event (character).}
#'   \item{Cardiac Arrest Etiology With Code (eArrest.02)}{Suspected cause of the cardiac arrest (character).}
#'   \item{Cardiac Arrest Indications Resuscitation Attempted By EMS (eArrest.03)}{Whether resuscitation was attempted by EMS (character).}
#'   \item{Cardiac Arrest Indications Resuscitation Attempted By EMS With Code List (eArrest.03)}{Detailed reasons for resuscitation decisions (character).}
#'   \item{Cardiac Arrest Witnessed By (eArrest.04)}{Who witnessed the cardiac arrest (character).}
#'   \item{Cardiac Arrest Witnessed By List (eArrest.04)}{List of all witnesses to the cardiac arrest (character).}
#'   \item{Cardiac Arrest CPR Provided Prior To EMS Arrival (3.4=eArrest.05/3.5=itArrest.105)}{Whether CPR was provided before EMS arrival (character).}
#'   \item{Cardiac Arrest Who Provided CPR Prior To EMS (3.4=eArrest.06/3.5=itArrest.106)}{Who performed CPR before EMS arrival (character).}
#'   \item{Cardiac Arrest Who Provided CPR Prior To EMS Arrival With Code List (3.4=eArrest.06/3.5=itArrest.106)}{List of responders who provided CPR before EMS arrival (character).}
#'   \item{Cardiac Arrest AED Use Prior To EMS Arrival (eArrest.07)}{Whether an AED was used before EMS arrival (character).}
#'   \item{Cardiac Arrest Who Used AED Prior To EMS (3.4=eArrest.08/3.5=itArrest.108)}{Who used the AED before EMS arrival (character).}
#'   \item{Cardiac Arrest Who Used AED Prior To EMS Arrival With Code List (3.4=eArrest.08/3.5=itArrest.108)}{List of responders who used the AED before EMS arrival (character).}
#'   \item{Cardiac Arrest Type Of CPR Provided List (eArrest.09)}{List of types of CPR performed (character).}
#'   \item{Cardiac Arrest First Monitored Arrest Rhythm Of Patient (eArrest.11)}{First recorded cardiac rhythm during arrest (character).}
#'   \item{Cardiac Arrest First Monitored Arrest Rhythm Of Patient With Code (eArrest.11)}{Coded representation of the first monitored cardiac rhythm (character).}
#'   \item{Cardiac Arrest Any Return Of Spontaneous Circulation (eArrest.12)}{Whether the patient regained spontaneous circulation (character).}
#'   \item{Cardiac Arrest Any Return Of Spontaneous Circulation With Code List (eArrest.12)}{List of codes indicating ROSC status (character).}
#'   \item{Cardiac Arrest Date Time (eArrest.14)}{Timestamp of cardiac arrest occurrence (datetime).}
#'   \item{Cardiac Arrest Resuscitation Discontinued Date Time (eArrest.15)}{Timestamp of resuscitation discontinuation (datetime).}
#'   \item{Cardiac Arrest Reason CPR Resuscitation Discontinued (eArrest.16)}{Reason for discontinuing resuscitation (character).}
#'   \item{Cardiac Arrest Rhythm On Arrival At Destination List (eArrest.17)}{Recorded cardiac rhythm upon arrival at the destination (character).}
#'   \item{Cardiac Arrest Patient Outcome At End Of EMS Event (eArrest.18)}{Patient's condition at the end of the EMS event (character).}
#'   \item{Cardiac Arrest Initial CPR Date Time (eArrest.19)}{Timestamp of initial CPR administration (datetime).}
#'   \item{Cardiac Arrest Who Initiated CPR With Code (3.4=itArrest.008/3.5=eArrest.20)}{Who initiated CPR (character).}
#'   \item{Cardiac Who First Applied The AED (3.4=itArrest.015/3.5=eArrest.21)}{Who first applied the AED (character).}
#'   \item{Cardiac Who First Defibrillated The Patient (3.4=itArrest.013/3.5=eArrest.22)}{Who performed the first defibrillation (character).}
#' }
#'
#' @usage data(nemsqar_arrest_table)
#'
#' @examples
#' data(nemsqar_arrest_table)
#' head(nemsqar_arrest_table)
#'
"nemsqar_arrest_table"
