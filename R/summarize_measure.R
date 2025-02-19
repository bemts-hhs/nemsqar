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
#' @author Samuel Kordik, BBA, BS
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
      numerator = sum({{numerator_col}}, na.rm = T),
      denominator = dplyr::n(),
      prop = sum(numerator / denominator, na.rm = T),
      prop_label = pretty_percent(prop,
                                  n_decimal = 2),
      ...
    )
}
