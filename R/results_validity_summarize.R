#' @title Summarize Validity Score Results
#'
#' @description
#'
#' Calculates validity score distribution summaries and validity score range
#' frequencies for one or more populations. Results are returned as a nested
#' list organized by analysis type (`distribution` and `counts`).
#'
#' When multiple populations are requested, population-specific results are
#' combined within each analysis type and identified by the `pop` variable.
#'
#' Distribution results contain descriptive statistics for validity scores,
#' while count results contain frequencies and proportions of observations
#' falling within predefined validity score ranges.
#'
#' @param total_population A dataframe or tibble containing the total
#'   population.
#' @param adult_population A dataframe or tibble containing the adult
#'   population.
#' @param peds_population A dataframe or tibble containing the pediatric
#'   population.
#' @param measure_name A string identifying the measure.
#' @param population_names Character vector indicating which populations
#'   to process. Options include `"all"`, `"adults"`, and `"peds"`.
#' @param validity_score_col Numeric validity score column.
#' @param validity_score_range_col Formatted validity score range column.
#' @param confidence_interval `r lifecycle::badge("experimental")`
#'   Logical indicating whether confidence intervals should be calculated for
#'   validity score range frequencies.
#' @param method `r lifecycle::badge("experimental")`
#'   Confidence interval method. Options are `"wilson"` and
#'   `"clopper-pearson"`.
#' @param conf.level `r lifecycle::badge("experimental")`
#'   Confidence level.
#' @param correct `r lifecycle::badge("experimental")`
#'   Should Wilson continuity correction be applied.
#' @param group_vars Optional character vector of subgroup variables used to
#'   calculate conditional distributions and subgroup frequency tables.
#'
#' @return A named list containing validity score summaries.
#'
#' - `distribution`: Validity score distribution summaries.
#'   - `overall`: Distribution summaries for the selected populations.
#'   - `stratified`: Distribution summaries stratified by `group_vars`
#'     (when requested).
#'
#' - `counts`: Validity score range counts and proportions.
#'   - `overall`: Count summaries for the selected populations.
#'   - `stratified`: Count summaries stratified by `group_vars`
#'     (when requested).
#'
#' When multiple populations are requested, results are combined into a single
#' tibble within each analysis type and distinguished by the `pop` variable.
#'
#' @author Nicolas Foss, Ed.D., MS
#'
#' @keywords internal
#'
#' @noRd
#'
results_validity_summarize <- function(
  total_population = NULL,
  adult_population = NULL,
  peds_population = NULL,
  measure_name,
  population_names = c("all", "adults", "peds"),
  validity_score_col,
  validity_score_range_col,
  confidence_interval = FALSE,
  method = c("wilson", "clopper-pearson"),
  conf.level = 0.95,
  correct = TRUE,
  group_vars = NULL
) {
  # Ensure method argument is valid ----
  method <- match.arg(method)

  # Ensure confidence interval is logical ----
  confidence_interval <- as.logical(confidence_interval)

  # Ensure population_names are valid ----
  valid_populations <- c("all", "adults", "peds")

  population_names <- match.arg(
    population_names,
    choices = valid_populations,
    several.ok = TRUE
  )

  # Population label lookup ----
  population_labels <- c(
    all = "All",
    adults = "Adults",
    peds = "Peds"
  )

  # Initialize output object ----
  results_list <- list()

  # Initialize collection objects ----

  ## distribution collection objects ----
  distribution_overall <- list()
  distribution_stratified <- list()

  ## counts collection objects ----
  counts_overall <- list()
  counts_stratified <- list()

  # Adults ----
  if ("adults" %in% population_names && !is.null(adult_population)) {
    adults <- list(
      distribution = summarize_validity_distribution(
        data = adult_population,
        measure_name = measure_name,
        population_name = population_labels[["adults"]],
        validity_score_col = {{ validity_score_col }},
        group_vars = group_vars
      ),
      counts = summarize_validity_bands(
        data = adult_population,
        measure_name = measure_name,
        population_name = population_labels[["adults"]],
        validity_score_col = {{ validity_score_range_col }},
        confidence_interval = confidence_interval,
        method = method,
        conf.level = conf.level,
        correct = correct,
        group_vars = group_vars
      )
    )

    ## fill the distribution object ----
    distribution_overall$adults <- adults$distribution$overall

    ## fill the count object ----
    counts_overall$adults <- adults$counts$overall

    ## optionally collect grouped data on the distribution ----
    if (!is.null(group_vars)) {
      distribution_stratified$adults <- adults$distribution$stratified
    }

    ## optionally collect grouped data on the counts ----
    if (!is.null(group_vars)) {
      counts_stratified$adults <- adults$counts$stratified
    }
  }

  # Pediatrics ----
  if ("peds" %in% population_names && !is.null(peds_population)) {
    peds <- list(
      distribution = summarize_validity_distribution(
        data = peds_population,
        measure_name = measure_name,
        population_name = population_labels[["peds"]],
        validity_score_col = {{ validity_score_col }},
        group_vars = group_vars
      ),
      counts = summarize_validity_bands(
        data = peds_population,
        measure_name = measure_name,
        population_name = population_labels[["peds"]],
        validity_score_col = {{ validity_score_range_col }},
        confidence_interval = confidence_interval,
        method = method,
        conf.level = conf.level,
        correct = correct,
        group_vars = group_vars
      )
    )

    ## fill the distribution object ----
    distribution_overall$peds <- peds$distribution$overall

    ## fill the counts object ----
    counts_overall$peds <- peds$counts$overall

    ## optionally collect grouped data on the distribution ----
    if (!is.null(group_vars)) {
      distribution_stratified$peds <- peds$distribution$stratified
    }

    ## optionally collect grouped data on the counts ----
    if (!is.null(group_vars)) {
      counts_stratified$peds <- peds$counts$stratified
    }
  }

  # All ----
  if ("all" %in% population_names && !is.null(total_population)) {
    all <- list(
      distribution = summarize_validity_distribution(
        data = total_population,
        measure_name = measure_name,
        population_name = population_labels[["all"]],
        validity_score_col = {{ validity_score_col }},
        group_vars = group_vars
      ),
      counts = summarize_validity_bands(
        data = total_population,
        measure_name = measure_name,
        population_name = population_labels[["all"]],
        validity_score_col = {{ validity_score_range_col }},
        confidence_interval = confidence_interval,
        method = method,
        conf.level = conf.level,
        correct = correct,
        group_vars = group_vars
      )
    )

    ## fill the distribution object ----
    distribution_overall$all <- all$distribution$overall

    ## fill the counts object ----
    counts_overall$all <- all$counts$overall

    ## optionally collect grouped data on the distribution ----
    if (!is.null(group_vars)) {
      distribution_stratified$all <- all$distribution$stratified
    }

    ## optionally collect grouped data on the counts ----
    if (!is.null(group_vars)) {
      counts_stratified$all <- all$counts$stratified
    }
  }

  # Bind distribution results ----
  results_list$distribution <- list(
    overall = dplyr::bind_rows(distribution_overall)
  )

  if (!is.null(group_vars)) {
    results_list$distribution$stratified <-
      dplyr::bind_rows(distribution_stratified)
  }

  # Bind count results ----
  results_list$counts <- list(
    overall = dplyr::bind_rows(counts_overall)
  )

  if (!is.null(group_vars)) {
    results_list$counts$stratified <-
      dplyr::bind_rows(counts_stratified)
  }

  # Return the results as a named nested list ----
  return(results_list)
}
