% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/airway_18_population.R
\name{airway_18_population}
\alias{airway_18_population}
\title{Airway-18 Populations}
\usage{
airway_18_population(
  df = NULL,
  patient_scene_table = NULL,
  procedures_table = NULL,
  vitals_table = NULL,
  airway_table = NULL,
  response_table = NULL,
  erecord_01_col,
  incident_date_col = NULL,
  patient_DOB_col = NULL,
  epatient_15_col,
  epatient_16_col,
  eresponse_05_col,
  eprocedures_01_col,
  eprocedures_02_col,
  eprocedures_03_col,
  eprocedures_06_col,
  eairway_02_col = NULL,
  eairway_04_col = NULL,
  evitals_01_col,
  evitals_16_col
)
}
\arguments{
\item{df}{A data frame or tibble containing the dataset to be processed.
Default is \code{NULL}.}

\item{patient_scene_table}{A data frame or tibble containing only ePatient
and eScene fields as a fact table. Default is \code{NULL}.}

\item{procedures_table}{A data frame or tibble containing only the
eProcedures fields needed for this measure's calculations. Default is
\code{NULL}.}

\item{vitals_table}{A data frame or tibble containing only the eVitals fields
needed for this measure's calculations. Default is \code{NULL}.}

\item{airway_table}{A data frame or tibble containing only the eAirway fields
needed for this measure's calculations. Default is \code{NULL}.}

\item{response_table}{A data frame or tibble containing only the eResponse
fields needed for this measure's calculations. Default is \code{NULL}.}

\item{erecord_01_col}{Column name containing the unique patient record
identifier.}

\item{incident_date_col}{Column that contains the incident date. This
defaults to \code{NULL} as it is optional in case not available due to PII
restrictions.}

\item{patient_DOB_col}{Column that contains the patient's date of birth. This
defaults to \code{NULL} as it is optional in case not available due to PII
restrictions.}

\item{epatient_15_col}{Column name for patient information (exact purpose
unclear).}

\item{epatient_16_col}{Column name for patient information (exact purpose
unclear).}

\item{eresponse_05_col}{Column name for emergency response codes.}

\item{eprocedures_01_col}{Column name for procedure times or other related
data.}

\item{eprocedures_02_col}{Column name for whether or not the procedure was
performed prior to EMS care being provided.}

\item{eprocedures_03_col}{Column name for procedure codes.}

\item{eprocedures_06_col}{Column name for procedure success codes.}

\item{eairway_02_col}{Column name for airway procedure data (datetime).
Default is \code{NULL}.}

\item{eairway_04_col}{Column name for airway procedure data. Default is
\code{NULL}.}

\item{evitals_01_col}{Column name for vital signs data (datetime).}

\item{evitals_16_col}{Column name for additional vital signs data.}
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

# If you are sourcing your data from a SQL database connection
# or if you have your data in several different tables,
# you can pass table inputs versus a single data.frame or tibble

# create tables to test correct functioning

  # patient table
  patient_table <- tibble::tibble(

    erecord_01 = rep(c("R1", "R2", "R3", "R4", "R5"), 2),
    incident_date = rep(as.Date(c("2025-01-01", "2025-01-05", "2025-02-01",
    "2025-01-01", "2025-06-01")), 2),
    patient_dob = rep(as.Date(c("2000-01-01", "2020-01-01", "2023-02-01",
                                "2023-01-01", "1970-06-01")), 2),
    epatient_15 = rep(c(25, 5, 2, 2, 55), 2),  # Ages
    epatient_16 = rep(c("Years", "Years", "Years", "Years", "Years"), 2)

  )

  # response table
  response_table <- tibble::tibble(

    erecord_01 = rep(c("R1", "R2", "R3", "R4", "R5"), 2),
    eresponse_05 = rep(2205001, 10)

  )

  # vitals table
  vitals_table <- tibble::tibble(

    erecord_01 = rep(c("R1", "R2", "R3", "R4", "R5"), 2),
    evitals_01 = lubridate::as_datetime(c("2025-01-01 23:02:00",
    "2025-01-05 12:03:00", "2025-02-01 19:04:00", "2025-01-01 05:05:00",
    "2025-06-01 13:01:00", "2025-01-01 23:02:00",
    "2025-01-05 12:03:00", "2025-02-01 19:04:00", "2025-01-01 05:05:00",
    "2025-06-01 13:06:00")),
    evitals_16 = rep(c(5, 6, 7, 8, 9), 2)

  )

  # airway table
  airway_table <- tibble::tibble(
  erecord_01 = rep(c("R1", "R2", "R3", "R4", "R5"), 2),
  eairway_02 = rep(lubridate::as_datetime(c("2025-01-01 23:05:00",
    "2025-01-05 12:02:00", "2025-02-01 19:03:00", "2025-01-01 05:04:00",
    "2025-06-01 13:06:00")), 2),
  eairway_04 = rep(4004019, 10)
  )

  # procedures table
  procedures_table <- tibble::tibble(

    erecord_01 = rep(c("R1", "R2", "R3", "R4", "R5"), 2),
    eprocedures_01 = rep(lubridate::as_datetime(c("2025-01-01 23:00:00",
    "2025-01-05 12:00:00", "2025-02-01 19:00:00", "2025-01-01 05:00:00",
    "2025-06-01 13:00:00")), 2),
    eprocedures_02 = rep("No", 10),
    eprocedures_03 = rep(c(16883004, 112798008, 78121007, 49077009,
                           673005), 2),
    eprocedures_06 = rep(9923003, 10)

  )

# Run the function
result <- airway_18_population(df = NULL,
         patient_scene_table = patient_table,
         procedures_table = procedures_table,
         vitals_table = vitals_table,
         response_table = response_table,
         airway_table = airway_table,
         erecord_01_col = erecord_01,
         incident_date_col = incident_date,
         patient_DOB_col = patient_dob,
         epatient_15_col = epatient_15,
         epatient_16_col = epatient_16,
         eresponse_05_col = eresponse_05,
         eprocedures_01_col = eprocedures_01,
         eprocedures_02_col = eprocedures_02,
         eprocedures_03_col = eprocedures_03,
         eprocedures_06_col = eprocedures_06,
         evitals_01_col = evitals_01,
         evitals_16_col = evitals_16,
         eairway_02_col = eairway_02,
         eairway_04_col = eairway_04
         )

# show the results of filtering at each step
result$filter_process

}
\author{
Nicolas Foss, Ed.D., MS

Nicolas Foss, Ed.D., MS, Samuel Kordik, BBA, BS
}
