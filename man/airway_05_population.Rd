% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/airway_05_population.R
\name{airway_05_population}
\alias{airway_05_population}
\title{Airway-05 Population}
\usage{
airway_05_population(
  df = NULL,
  patient_scene_table = NULL,
  response_table = NULL,
  arrest_table = NULL,
  procedures_table = NULL,
  vitals_table = NULL,
  erecord_01_col,
  incident_date_col = NULL,
  patient_DOB_col = NULL,
  epatient_15_col,
  epatient_16_col,
  earrest_01_col,
  eresponse_05_col,
  evitals_01_col,
  evitals_12_col,
  eprocedures_01_col,
  eprocedures_02_col,
  eprocedures_03_col
)
}
\arguments{
\item{df}{A dataframe or tibble contianing EMS data where each row represents
an observation and columns represent features. Default is \code{NULL}.}

\item{patient_scene_table}{A data.frame or tibble containing at least
epatient, escene, and earrest.01 fields as a fact table. Default is \code{NULL}.}

\item{response_table}{A data.frame or tibble containing at least the
eresponse fields needed for this measure's calculations. Default is \code{NULL}.}

\item{arrest_table}{A data.frame or tibble containing at least the earrest
fields needed for this measure's calculations. Default is \code{NULL}.}

\item{procedures_table}{A dataframe or tibble containing at least the
eProcedures fields needed. Default is \code{NULL}.}

\item{vitals_table}{A dataframe or tibble containing at least the eVitals
fields needed. Default is \code{NULL}.}

\item{erecord_01_col}{The column representing the EMS record unique
identifier.}

\item{incident_date_col}{Column that contains the incident date. This
defaults to \code{NULL} as it is optional in case not available due to PII
restrictions.}

\item{patient_DOB_col}{Column that contains the patient's date of birth. This
defaults to \code{NULL} as it is optional in case not available due to PII
restrictions.}

\item{epatient_15_col}{Column representing the patient's numeric age agnostic
of unit.}

\item{epatient_16_col}{Column representing the patient's age unit ("Years",
"Months", "Days", "Hours", or "Minutes").}

\item{earrest_01_col}{Column representing whether or not the patient is in
arrest.}

\item{eresponse_05_col}{Column that contains eResponse.05.}

\item{evitals_01_col}{Date-time or POSIXct column containing vital signs
date/time}

\item{evitals_12_col}{Numeric column containing pulse oximetry values.}

\item{eprocedures_01_col}{Date-time or POSIXct column for procedures}

\item{eprocedures_02_col}{Column name for whether or not the procedure was
performed prior to EMS care being provided.}

\item{eprocedures_03_col}{Column containing procedure codes with or without
procedure names.}
}
\value{
A list that contains the following:
\itemize{
\item a tibble with counts for each filtering step,
\item a tibble for each population of interest
\item a tibble for the initial population
\item a tibble for the total dataset with computations
}
}
\description{
This function processes and analyzes the dataset to generate the populations
of interest needed to perform calculations to obtain performance data.
}
\examples{

\dontrun{

# If you are sourcing your data from a SQL database connection
# or if you have your data in several different tables,
# you can pass table inputs versus a single data.frame or tibble

# Get the applicable tables from `nemsqar`
data("nemsqar_arrest_table")
data("nemsqar_patient_scene_table")
data("nemsqar_response_table")
data("nemsqar_vitals_table")
data("nemsqar_procedures_table")

# Run the function

airway_05_population(df = NULL,
         patient_scene_table = nemsqar_patient_scene_table,
         procedures_table = nemsqar_procedures_table,
         vitals_table = nemsqar_vitals_table,
         arrest_table = nemsqar_arrest_table,
         response_table = nemsqar_response_table,
         erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
         incident_date_col = `Incident Date`,
         patient_DOB_col = `Patient Date Of Birth (ePatient.17)`,
         epatient_15_col = `Patient Age (ePatient.15)`,
         epatient_16_col = `Patient Age Units (ePatient.16)`,
         eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
         eprocedures_01_col = `Procedure Performed Date Time (eProcedures.01)`,
         eprocedures_02_col = `Procedure Performed Prior To EMS Care (eProcedures.02)`,
         eprocedures_03_col = `Procedure Performed Description And Code (eProcedures.03)`,
         earrest_01_col = `Cardiac Arrest During EMS Event With Code (eArrest.01)`,
         evitals_01_col = `Vitals Signs Taken Date Time (eVitals.01)`,
         evitals_012_col = `Vitals Pulse Oximetry (eVitals.12)`
         )
}

}
\author{
Samuel Kordik, BBA, BS, Nicolas Foss Ed.D., MS
}
