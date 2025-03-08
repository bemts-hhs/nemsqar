% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/syncope_01.R
\name{syncope_01}
\alias{syncope_01}
\title{Syncope-01 Calculation}
\usage{
syncope_01(
  df = NULL,
  patient_scene_table = NULL,
  response_table = NULL,
  situation_table = NULL,
  vitals_table = NULL,
  erecord_01_col,
  incident_date_col = NULL,
  patient_DOB_col = NULL,
  epatient_15_col,
  epatient_16_col,
  eresponse_05_col,
  esituation_09_col,
  esituation_10_col,
  esituation_11_col,
  esituation_12_col,
  evitals_04_col,
  ...
)
}
\arguments{
\item{df}{Main data frame containing EMS records.}

\item{patient_scene_table}{A data frame or tibble containing only epatient
and escene fields as a fact table. Default is \code{NULL}.}

\item{response_table}{A data frame or tibble containing only the eresponse
fields needed for this measure's calculations. Default is \code{NULL}.}

\item{situation_table}{A data.frame or tibble containing only the esituation
fields needed for this measure's calculations. Default is \code{NULL}.}

\item{vitals_table}{A data.frame or tibble containing only the evitals fields
needed for this measure's calculations. Default is \code{NULL}.}

\item{erecord_01_col}{The column containing unique record identifiers for
each encounter.}

\item{incident_date_col}{Column that contains the incident date. This
defaults to \code{NULL} as it is optional in case not available due to PII
restrictions.}

\item{patient_DOB_col}{Column that contains the patient's date of birth. This
defaults to \code{NULL} as it is optional in case not available due to PII
restrictions.}

\item{epatient_15_col}{Column representing the patient age (numeric).}

\item{epatient_16_col}{Column for the patient age units (e.g., "Years",
"Months").}

\item{eresponse_05_col}{Column containing response type codes, specifically
911 codes.}

\item{esituation_09_col}{Column with primary symptoms associated with the
patient encounter.}

\item{esituation_10_col}{Column with other associated symptoms.}

\item{esituation_11_col}{Column for primary impression code.}

\item{esituation_12_col}{Column for secondary impression codes.}

\item{evitals_04_col}{Column with ECG information if available.}

\item{...}{Additional arguments passed to \code{dplyr::summarize} for grouped
summaries.}
}
\value{
A tibble summarizing results for three population groups (Adults, and
Peds) with the following columns:

\code{measure}: The name of the measure being calculated.
\code{pop}: Population type (Adults, Peds).
\code{numerator}: Count of incidents where beta-agonist medications were
administered.
\code{denominator}: Total count of incidents.
\code{prop}: Proportion of incidents involving beta-agonist medications.
\code{prop_label}: Proportion formatted as a percentage with a specified number
of decimal places.
}
\description{
The \code{syncope_01} function processes EMS dataset to identify potential syncope
(fainting) cases based on specific criteria and calculates related ECG
measures. This function dplyr::filters data for 911 response calls, assesses
primary and associated symptoms for syncope, determines age-based populations
(adult and pediatric), and aggregates results by unique patient encounters.
}
\examples{

# Synthetic test data
  test_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    esituation_09 = c(rep("R55", 3), rep("R40.4", 2)),
    esituation_10 = c(rep("R40.4", 2), rep("R55", 3)),
    esituation_11 = c(rep("R55", 3), rep("R40.4", 2)),
    esituation_12 = c(rep("R40.4", 2), rep("R55", 3)),
    evitals_04 = rep("15 Lead", 5)
  )

  # Run function
  syncope_01(
    df = test_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    esituation_09_col = esituation_09,
    esituation_10_col = esituation_10,
    esituation_11_col = esituation_11,
    esituation_12_col = esituation_12,
    evitals_04_col = evitals_04
  )

}
\author{
Nicolas Foss, Ed.D., MS
}
