% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trauma_01.R
\name{trauma_01}
\alias{trauma_01}
\title{Trauma-01 Calculation}
\usage{
trauma_01(
  df = NULL,
  patient_scene_table = NULL,
  response_table = NULL,
  situation_table = NULL,
  disposition_table = NULL,
  vitals_table = NULL,
  erecord_01_col,
  incident_date_col = NULL,
  patient_DOB_col = NULL,
  epatient_15_col,
  epatient_16_col,
  esituation_02_col,
  eresponse_05_col,
  evitals_23_col,
  evitals_26_col,
  evitals_27_col,
  edisposition_28_col,
  transport_disposition_col,
  confidence_interval = FALSE,
  method = c("wilson", "clopper-pearson"),
  conf.level = 0.95,
  correct = TRUE,
  ...
)
}
\arguments{
\item{df}{A data frame or tibble containing EMS records. Default is \code{NULL}.}

\item{patient_scene_table}{A data frame or tibble containing only epatient
and escene fields as a fact table. Default is \code{NULL}.}

\item{response_table}{A data frame or tibble containing only the eresponse
fields needed for this measure's calculations. Default is \code{NULL}.}

\item{situation_table}{A data.frame or tibble containing only the esituation
fields needed for this measure's calculations. Default is \code{NULL}.}

\item{disposition_table}{A data.frame or tibble containing only the
edisposition fields needed for this measure's calculations. Default is
\code{NULL}.}

\item{vitals_table}{A data.frame or tibble containing only the evitals fields
needed for this measure's calculations. Default is \code{NULL}.}

\item{erecord_01_col}{Column name representing the EMS record ID.}

\item{incident_date_col}{Column that contains the incident date. This
defaults to \code{NULL} as it is optional in case not available due to PII
restrictions.}

\item{patient_DOB_col}{Column that contains the patient's date of birth. This
defaults to \code{NULL} as it is optional in case not available due to PII
restrictions.}

\item{epatient_15_col}{Column name for the patient's age in numeric format.}

\item{epatient_16_col}{Column name for the unit of age (e.g., "Years",
"Months").}

\item{esituation_02_col}{Column name indicating if the situation involved an
injury.}

\item{eresponse_05_col}{Column name for the type of EMS response (e.g., 911
call).}

\item{evitals_23_col}{Column name for the Glasgow Coma Scale (GCS) total
score.}

\item{evitals_26_col}{Column name for AVPU (Alert, Voice, Pain, Unresponsive)
status.}

\item{evitals_27_col}{Column name for the pain scale assessment.}

\item{edisposition_28_col}{Column name for patient care disposition details.}

\item{transport_disposition_col}{Column name for transport disposition
details.}

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
This function processes EMS data to calculate the Trauma-01 performance
measure, which evaluates the percentage of trauma patients assessed for pain
using a numeric scale. The function filters and summarizes the data based on
specified inclusion criteria.
}
\examples{

# Synthetic test data
  test_data <- tibble::tibble(
    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    epatient_15 = c(34, 5, 45, 2, 60),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Months", "Years"),
    eresponse_05 = rep(2205001, 5),
    esituation_02 = rep("Yes", 5),
    evitals_23 = rep(15, 5),
    evitals_26 = rep("Alert", 5),
    evitals_27 = c(0, 2, 4, 6, 8),
    edisposition_28 = rep(4228001, 5),
    edisposition_30 = c(4230001, 4230003, 4230001, 4230007, 4230007)
  )

# Run the function
# Return 95\% confidence intervals using the Wilson method
  trauma_01(
    df = test_data,
    erecord_01_col = erecord_01,
    epatient_15_col = epatient_15,
    epatient_16_col = epatient_16,
    eresponse_05_col = eresponse_05,
    esituation_02_col = esituation_02,
    evitals_23_col = evitals_23,
    evitals_26_col = evitals_26,
    evitals_27_col = evitals_27,
    edisposition_28_col = edisposition_28,
    transport_disposition_col = edisposition_30,
    confidence_interval = TRUE
  )

}
\author{
Nicolas Foss, Ed.D., MS
}
