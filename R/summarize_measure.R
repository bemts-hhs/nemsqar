#' Summarize Measure
#'
#' Calculates measure numerator, denominator, and proportions
#' for a NEMSQA measure.
#'
#' Used throughout this package to calculate measure summaries.
#'
#' @param data a dataframe or tibble containing the filtered and calculated fields.
#' @param measure_name A string containing the measure description.
#' @param population_name A string containing the population description.
#' @param numerator_col The tidyselect column containing the numerator.
#' @param ... (optional) additional arguments
#'
#' @return Summarized information
#' 
#' @section Credit:
#' 
#' This function was developed by (Samuel Kordik, BBA, BS)[https://www.linkedin.com/in/samuelkordik] at Dallas Fire-Rescue Office of the Medical
#' Director.
#' 
#' @export
#' 
summarize_measure <- function(data,
                              measure_name,
                              population_name,
                              numerator_col,
                              ...) {

  data |>
    dplyr::summarize(
      measure = measure_name,
      pop = population_name,
      numerator = sum({{numerator_col}}, na.rm=T),
      denominator = dplyr::n(),
      prop = sum(numerator / denominator),
      prop_label = pretty_percent(prop,
                                  n_decimal = 0.01),
      ...
    )
}
