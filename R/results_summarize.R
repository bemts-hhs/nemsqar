#' Tabulate Measure Results
#'
#' Calculates measure numerator, denominator, and proportions
#' for a NEMSQA measure and each population.
#'
#' Used throughout this package to calculate measure results.
#'
#' @param total_population a dataframe or tibble containing the filtered and calculated fields.
#' @param adult_population a dataframe or tibble containing the filtered and calculated fields.
#' @param peds_population a dataframe or tibble containing the filtered and calculated fields.
#' @param measure_name A string containing the measure description.
#' @param population_names A list containing the population descriptions.
#' @param numerator_col The tidyselect column containing the numerator.
#' @param ... (optional) additional arguments
#'
#' @return Results tibble
#' 
#' @author Samuel Kordik, BBA, BS
#' 
#' @export
#' 
results_summarize <- function(total_population,
                              adult_population,
                              peds_population,
                              measure_name,
                              population_names = c(total = "All",
                                                   adult = "Adults",
                                                   peds = "Peds"),
                              numerator_col,
                              ...) {
  population_names = c(total = "All",
                       adult = "Adults",
                       peds = "Peds")

  dplyr::bind_rows(
    adult_population |> summarize_measure(measure_name,
                                          population_names[["adult"]],
                                          {{numerator_col}},
                                          ...),
    peds_population |> summarize_measure(measure_name,
                                         population_names[["peds"]],
                                         {{numerator_col}},
                                          ...),
    total_population |> summarize_measure(measure_name,
                                          population_names[["total"]],
                                          {{numerator_col}},
                                          ...)
  )

}
