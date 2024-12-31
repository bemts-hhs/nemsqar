#' Safety-02 Calculation
#'
#' The `safety_02` function calculates the Safety-02 metric, evaluating the
#' proportion of emergency medical calls involving transport where no lights and
#' sirens were used. This function categorizes the population into adult and
#' pediatric groups based on their age, and summarizes results with a total
#' population count as well.
#'
#' @section Details:
#'
#' Assume data are already loaded
#' Need to be a table where each row is 1 observation and each column is a feature
#' or distinct datasets that can be referenced as unique columns
#' this function will calculate an age in years
#' this function also assumes that rows that are missing any value are NA,
#' not the not known / not recorded values common to ImageTrend or the value codes
#' that correspond to "not values".
#' the function assumes that the eresponse.05 column has the codes in it, text
#' can be present, too, for reference
#' the function assumes that edisposition.18 is a list column or a column that has all
#' text descriptors for additional transport mode descriptors.  These can be separated
#' by commas or other characters as long as all eresponse.18 values are present
#' in one cell for each unique erecord.01 value.  Codes can be present
#' but will be ignored by the function.
#' for the argument transport_disposition_cols, this argument can receive the unquoted
#' column names of edisposition.12 and edisposition.30.  One or both can be entered and
#' the function will evaluate them.  These columns are used to create a `transport`
#' variable that is used to filter the table down furhter.  AS such, these columns
#' edisposition.12 and edisposition.30 must be list columns that and/or contain all values
#' from each unique incident entered for each field.  These can be comma separated values
#' all in one cell to make the table tidy.
#' the first argument is a dataframe, no joining is done.
#' any joins to get vitals etc. will need to be done outside the function
#' Grouping by specific attributes (e.g., region) can be performed inside this function by
#' utilizing the `.by` argument passed via tidydots (i.e. `...`) to `dplyr::summarize`.
#'
#' @param df A data frame where each row is an observation, and each column represents a feature.
#' @param patient_scene_table A data.frame or tibble containing only epatient and escene fields as a fact table.
#' @param response_table A data.frame or tibble containing only the eresponse fields needed for this measure's calculations.
#' @param disposition_table A data.frame or tibble containing only the edisposition fields needed for this measure's calculations.
#' @param incident_date_col <['tidy-select'][dplyr_tidy_select]> Date or POSIXct Unquoted column name representing the date of the incident.
#' @param patient_DOB_col <['tidy-select'][dplyr_tidy_select]> Date or POSIXct Unquoted column name for the patient's date of birth.
#' @param epatient_15_col <['tidy-select'][dplyr_tidy_select]> Column giving the calculated age value.
#' @param epatient_16_col <['tidy-select'][dplyr_tidy_select]> Column giving the provided age unit value.
#' @param eresponse_05_col <['tidy-select'][dplyr_tidy_select]> Column giving response codes, identifying 911 responses.
#' @param edisposition_18_col <['tidy-select'][dplyr_tidy_select]> Column giving transport mode descriptors, including possible lights-and-sirens indicators.
#' @param edisposition_28_col <['tidy-select'][dplyr_tidy_select]> Column giving patient evaluation and care categories for the EMS response.
#' @param transport_disposition_cols <['tidy-select'][dplyr_tidy_select]> One or more unquoted column names (such as edisposition.12, edisposition.30) containing transport 
#' disposition details.
#' @param ... Additional arguments for summary calculation, if needed.
#'
#' @return A tibble summarizing results for three age groups (< 10 yrs, 10–65 yrs, and >= 65 yrs) with the following columns:
#'
#' `pop`: Population type (< 18 yrs, >= 18 yrs, all).
#' `numerator`: Count of incidents from a 911 request during which lights and sirens were not used during patient 
#' transport.
#' `denominator`: Total count of incidents.
#' `prop`: Proportion of incidents from a 911 request during which lights and sirens were not used during patient 
#' transport.
#' `prop_label`: Proportion formatted as a percentage with a specified number of decimal places.
#' 
#' @author Nicolas Foss, Ed.D., MS
#' 
#' @export
#' 
safety_02 <- function(df = NULL,
                      patient_scene_table = NULL,
                      response_table = NULL,
                      disposition_table = NULL,
                      erecord_01_col,
                      incident_date_col = NULL,
                      patient_DOB_col = NULL,
                      epatient_15_col,
                      epatient_16_col,
                      eresponse_05_col,
                      edisposition_18_col,
                      edisposition_28_col,
                      transport_disposition_cols,
                      ...) {


  # utilize applicable tables to analyze the data for the measure
  if (
    all(
      !is.null(patient_scene_table), 
      !is.null(response_table),
      !is.null(disposition_table)
    ) && is.null(df)
    
  ) {
    
    # header
    cli::cli_h1("Safety-02")
    
    # header
    cli::cli_h2("Gathering Records for Safety-02")
    
    # gather the population of interest
    safety_02_populations <- safety_02_population(patient_scene_table = patient_scene_table,
                                                  response_table = response_table,
                                                  disposition_table = disposition_table,
                                                  erecord_01_col = {{ erecord_01_col }},
                                                  incident_date_col = {{ incident_date_col }},
                                                  patient_DOB_col = {{ patient_DOB_col }},
                                                  epatient_15_col = {{ epatient_15_col }},
                                                  epatient_16_col = {{ epatient_16_col }},
                                                  eresponse_05_col = {{ eresponse_05_col }},
                                                  edisposition_18_col = {{ edisposition_18_col }},
                                                  edisposition_28_col = {{ edisposition_28_col }},
                                                  transport_disposition_cols = {{ transport_disposition_cols }}
                                                  )
    
    # create a separator
    cli::cli_text("\n")
    
    # header for calculations
    cli::cli_h2("Calculating Safety-02")
    
    # summary
    safety.02 <- results_summarize(total_population = safety_02_populations$initial_population,
                                   adult_population = safety_02_populations$adults,
                                   peds_population = safety_02_populations$peds,
                                   measure_name = "Safety-02",
                                   numerator_col = NO_LS_CHECK,
                                   ...)
    
    # create a separator
    cli::cli_text("\n")
    
    return(safety.02)
    
  } else if(
    
    all(
      is.null(patient_scene_table), 
      is.null(response_table),
      is.null(disposition_table)
    ) && !is.null(df)
    
    # utilize a dataframe to analyze the data for the measure analytics
    
  ) {
  
    # header
    cli::cli_h1("Safety-02")
    
    # header
    cli::cli_h2("Gathering Records for Safety-02")
    
    # gather the population of interest
    safety_02_populations <- safety_02_population(df = df,
                                                  erecord_01_col = {{ erecord_01_col }},
                                                  incident_date_col = {{ incident_date_col }},
                                                  patient_DOB_col = {{ patient_DOB_col }},
                                                  epatient_15_col = {{ epatient_15_col }},
                                                  epatient_16_col = {{ epatient_16_col }},
                                                  eresponse_05_col = {{ eresponse_05_col }},
                                                  edisposition_18_col = {{ edisposition_18_col }},
                                                  edisposition_28_col = {{ edisposition_28_col }},
                                                  transport_disposition_cols = {{ transport_disposition_cols }}
                                                  )
    
    # create a separator
    cli::cli_text("\n")
    
    # header for calculations
    cli::cli_h2("Calculating Safety-02")
    
    # summary
    safety.02 <- results_summarize(total_population = safety_02_populations$initial_population,
                                   adult_population = safety_02_populations$adults,
                                   peds_population = safety_02_populations$peds,
                                   measure_name = "Safety-02",
                                   numerator_col = NO_LS_CHECK,
                                   ...)
    
    # create a separator
    cli::cli_text("\n")
    
    return(safety.02)
    
  }
  
}
