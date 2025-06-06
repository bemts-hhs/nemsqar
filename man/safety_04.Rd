% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/safety_04.R
\name{safety_04}
\alias{safety_04}
\title{Safety-04 Calculation}
\usage{
safety_04(
  df = NULL,
  patient_scene_table = NULL,
  response_table = NULL,
  arrest_table = NULL,
  injury_table = NULL,
  procedures_table = NULL,
  disposition_table = NULL,
  erecord_01_col,
  incident_date_col = NULL,
  patient_DOB_col = NULL,
  epatient_15_col,
  epatient_16_col,
  eresponse_05_col,
  earrest_01_col,
  einjury_03_col,
  eprocedures_03_col,
  edisposition_14_col,
  transport_disposition_col,
  confidence_interval = FALSE,
  method = c("wilson", "clopper-pearson"),
  conf.level = 0.95,
  correct = TRUE,
  ...
)
}
\arguments{
\item{df}{A data frame or tibble containing EMS data where each row
represents an individual observation.}

\item{patient_scene_table}{A data frame or tibble containing fields from
epatient and escene needed for this measure's calculations.}

\item{response_table}{A data frame or tibble containing fields from eresponse
needed for this measure's calculations.}

\item{arrest_table}{A data frame or tibble containing fields from earrest
needed for this measure's calculations.}

\item{injury_table}{A data frame or tibble containing fields from einjury
needed for this measure's calculations.}

\item{procedures_table}{A data frame or tibble containing fields from
eprocedures needed for this measure's calculations.}

\item{disposition_table}{A data frame or tibble containing fields from
edisposition needed for this measure's calculations.}

\item{erecord_01_col}{The column containing unique record identifiers for
each encounter.}

\item{incident_date_col}{Column that contains the incident date. This
defaults to \code{NULL} as it is optional in case not available due to PII
restrictions.}

\item{patient_DOB_col}{Column that contains the patient's date of birth. This
defaults to \code{NULL} as it is optional in case not available due to PII
restrictions.}

\item{epatient_15_col}{Column name indicating the patient age.}

\item{epatient_16_col}{Column name for the unit of age (e.g., "Years,"
"Months").}

\item{eresponse_05_col}{Column containing response transport codes.}

\item{earrest_01_col}{Column with cardiac arrest status information.}

\item{einjury_03_col}{Column describing traumatic injuries, expected as a
list or text-separated entries.}

\item{eprocedures_03_col}{Column listing procedures, assumed to contain
multiple procedure codes/texts in each cell.}

\item{edisposition_14_col}{Column for transport dispositions.}

\item{transport_disposition_col}{Columns for primary and secondary transport
dispositions.}

\item{confidence_interval}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}} Logical. If
\code{TRUE}, the function calculates a confidence interval for the proportion
estimate.}

\item{method}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}Character. Specifies the
method used to calculate confidence intervals. Options are \code{"wilson"}
(Wilson score interval) and \code{"clopper-pearson"} (exact binomial interval).
Partial matching is supported, so \code{"w"} and \code{"c"} can be used as shorthand.}

\item{conf.level}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}Numeric. The confidence
level for the interval, expressed as a proportion (e.g., 0.95 for a 95\%
confidence interval). Defaults to 0.95.}

\item{correct}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}Logical. If \code{TRUE},
applies a continuity correction to the Wilson score interval when \code{method = "wilson"}. Defaults to \code{TRUE}.}

\item{...}{optional additional arguments to pass onto \code{dplyr::summarize}.}
}
\value{
A data.frame summarizing results for two population groups (All,
Adults and Peds) with the following columns:
\itemize{
\item \code{pop}: Population type (All, Adults, and Peds).
\item \code{numerator}: Count of incidents meeting the measure.
\item \code{denominator}: Total count of included incidents.
\item \code{prop}: Proportion of incidents meeting the measure.
\item \code{prop_label}: Proportion formatted as a percentage with a specified number
of decimal places.
\item \code{lower_ci}: Lower bound of the confidence interval for \code{prop}
(if \code{confidence_interval = TRUE}).
\item \code{upper_ci}: Upper bound of the confidence interval for \code{prop}
(if \code{confidence_interval = TRUE}).
}
}
\description{
The \code{safety_04} function processes EMS incident data for specific safety and
transport criteria, filtering by patient age and incident type to identify
cases that meet specified exclusion or inclusion criteria. This function
accommodates data with various EMS-specific codes, age descriptors, and
procedure identifiers.
}
\examples{

# Synthetic test data
  test_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    earrest_01 = rep("No", 5),
    einjury_03 = rep("non-injury", 5),
    edisposition_14 = rep(4214001, 5),
    edisposition_30 = rep(4230001, 5),
    eprocedures_03 = rep("other response", 5)
  )

# Run the function
# Return 95\% confidence intervals using the Wilson method
  safety_04(
    df = test_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    earrest_01_col = earrest_01,
    einjury_03_col = einjury_03,
    edisposition_14_col = edisposition_14,
    transport_disposition_col = edisposition_30,
    eprocedures_03_col = eprocedures_03,
    confidence_interval = TRUE
  )

}
\author{
Nicolas Foss, Ed.D., MS
}
