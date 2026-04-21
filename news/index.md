# Changelog

## nemsqar 1.1.3

- Added navigation to all major functions in the package. This was done
  by adding `----` to the end of most comments.
- Some functions received code formatting due to the addition of the
  `----` or just by virtue of editing the script, the `Air` package does
  the formatting via Positron.
- Improved comments in the code in some functions and removed one code
  redundancy in a function that performed the exact same check that the
  `*_table` and `df` arguments were receiving inputs simulataneously.

## nemsqar 1.1.2

CRAN release: 2025-07-21

- Fixed broken URLs in the documentation for
  [`nemsqa_binomial_confint()`](https://bemts-hhs.github.io/nemsqar/reference/nemsqa_binomial_confint.md).

## nemsqar 1.1.1

CRAN release: 2025-07-16

- In
  [`airway_01_population()`](https://bemts-hhs.github.io/nemsqar/reference/airway_01_population.md),
  the filter_process object had one text descriptor
  `"All initial population successful intubation with no hypoxia or hypoxia/hypotension"`
  which was corrected to be
  `"All initial population successful intubation with no hypoxia/hypotension"`.

- Also, in
  [`trauma_14_population()`](https://bemts-hhs.github.io/nemsqar/reference/trauma_14_population.md),
  the `filter_process` object has one text descriptor
  `"Tournique procedure"` which was corrected to be
  `"Tourniquet procedure"`.

- Additionally, trauma_14_population() should *just work* after passing
  an arbitrary number of (applicable) columns to the
  `transport_disposition` argument. In {nemsqar} 1.1.0, only one column
  will work as the [`grepl()`](https://rdrr.io/r/base/grep.html) call is
  not wrapped in `dpyr::if_any()`. This was fixed so that users can
  reference other columns that contain transport disposition data, such
  as eDisposition.12 from NEMSIS 3.4 (or earlier versions as
  applicable). This helps with back compatibility when using this
  measure to earlier versions of NEMSIS, or later versions.

## nemsqar 1.1.0

CRAN release: 2025-03-13

### Enhancements

- **Optional Confidence Intervals**: Added the ability to compute
  confidence intervals using the Wilson or Clopper-Pearson (exact)
  method. This feature is optional and can be enabled when working with
  sample data.
  - Introduced
    [`nemsqa_binomial_confint()`](https://bemts-hhs.github.io/nemsqar/reference/nemsqa_binomial_confint.md),
    a lightweight wrapper around
    [`prop.test()`](https://rdrr.io/r/stats/prop.test.html) and
    [`binom.test()`](https://rdrr.io/r/stats/binom.test.html) for
    calculating Wilson and exact confidence intervals. This function
    eliminates the need for an additional package dependency.
    - Ensure warning messages where any `denominator` \< 10 are elegant
      and helpful, and `nemsqa_binomial_confit()` handles division by
      zero cases well.
  - Updated all wrapper functions (e.g.,
    [`airway_01()`](https://bemts-hhs.github.io/nemsqar/reference/airway_01.md))
    to support optional confidence interval calculation.
  - Maintained full backward compatibility with **nemsqar 1.0.0** by
    setting `confidence_interval = FALSE` as the default behavior.
- **Dynamic
  [`results_summarize()`](https://bemts-hhs.github.io/nemsqar/reference/results_summarize.md)**:
  Enhanced
  [`results_summarize()`](https://bemts-hhs.github.io/nemsqar/reference/results_summarize.md)
  to dynamically calculate only the specified groups, utilizing the
  previously unused `population_labels` object. This reduces unnecessary
  calculations and streamlines function performance.  
- **Improved Documentation**:
  - Updated and expanded the documentation for
    [`results_summarize()`](https://bemts-hhs.github.io/nemsqar/reference/results_summarize.md)
    and
    [`summarize_measure()`](https://bemts-hhs.github.io/nemsqar/reference/summarize_measure.md),
    offering clearer usage instructions and examples to enhance the user
    experience.
  - Refined the documentation for multiple other functions, improving
    clarity and usability.

## nemsqar 1.0.0

CRAN release: 2025-03-05

### Initial CRAN Release

- First official submission of **nemsqar** to CRAN.

## nemsqar 0.1.0

### Package Inception

- **nemsqar is born!** This initial version laid the foundation for
  calculating National EMS Quality Alliance (NEMSQA) performance
  measures in a structured and modular way.

#### Key Features

- Designed core functions to **identify target populations** and
  **compute performance measures** for EMS quality metrics.  
- Implemented a modular structure for measure calculations, with
  `_population` workhorse functions handling data extraction and
  `measure_##` wrapper functions streamlining performance
  calculations.  
- Developed functions to align with **NEMSQA measure technical
  documents**.

#### Implemented Functions

##### Measure Functions

- [`airway_01()`](https://bemts-hhs.github.io/nemsqar/reference/airway_01.md),
  [`airway_05()`](https://bemts-hhs.github.io/nemsqar/reference/airway_05.md),
  [`airway_18()`](https://bemts-hhs.github.io/nemsqar/reference/airway_18.md)  
- [`asthma_01()`](https://bemts-hhs.github.io/nemsqar/reference/asthma_01.md),
  [`hypoglycemia_01()`](https://bemts-hhs.github.io/nemsqar/reference/hypoglycemia_01.md),
  [`pediatrics_03b()`](https://bemts-hhs.github.io/nemsqar/reference/pediatrics_03b.md)  
- [`respiratory_01()`](https://bemts-hhs.github.io/nemsqar/reference/respiratory_01.md),
  [`respiratory_02()`](https://bemts-hhs.github.io/nemsqar/reference/respiratory_02.md),
  [`safety_01()`](https://bemts-hhs.github.io/nemsqar/reference/safety_01.md)  
- [`safety_02()`](https://bemts-hhs.github.io/nemsqar/reference/safety_02.md),
  [`safety_04()`](https://bemts-hhs.github.io/nemsqar/reference/safety_04.md),
  [`seizure_02()`](https://bemts-hhs.github.io/nemsqar/reference/seizure_02.md)  
- [`stroke_01()`](https://bemts-hhs.github.io/nemsqar/reference/stroke_01.md),
  [`syncope_01()`](https://bemts-hhs.github.io/nemsqar/reference/syncope_01.md),
  [`tbi_01()`](https://bemts-hhs.github.io/nemsqar/reference/tbi_01.md)  
- [`trauma_01()`](https://bemts-hhs.github.io/nemsqar/reference/trauma_01.md),
  [`trauma_03()`](https://bemts-hhs.github.io/nemsqar/reference/trauma_03.md),
  [`trauma_04()`](https://bemts-hhs.github.io/nemsqar/reference/trauma_04.md)  
- [`trauma_08()`](https://bemts-hhs.github.io/nemsqar/reference/trauma_08.md),
  [`trauma_14()`](https://bemts-hhs.github.io/nemsqar/reference/trauma_14.md),
  [`ttr_01()`](https://bemts-hhs.github.io/nemsqar/reference/ttr_01.md)

##### Population Functions

- [`airway_01_population()`](https://bemts-hhs.github.io/nemsqar/reference/airway_01_population.md),
  [`airway_05_population()`](https://bemts-hhs.github.io/nemsqar/reference/airway_05_population.md),
  [`airway_18_population()`](https://bemts-hhs.github.io/nemsqar/reference/airway_18_population.md)  
- [`asthma_01_population()`](https://bemts-hhs.github.io/nemsqar/reference/asthma_01_population.md),
  [`hypoglycemia_01_population()`](https://bemts-hhs.github.io/nemsqar/reference/hypoglycemia_01_population.md),
  [`pediatrics_03b_population()`](https://bemts-hhs.github.io/nemsqar/reference/pediatrics_03b_population.md)  
- [`respiratory_01_population()`](https://bemts-hhs.github.io/nemsqar/reference/respiratory_01_population.md),
  [`respiratory_02_population()`](https://bemts-hhs.github.io/nemsqar/reference/respiratory_02_population.md),
  [`safety_01_population()`](https://bemts-hhs.github.io/nemsqar/reference/safety_01_population.md)  
- [`safety_02_population()`](https://bemts-hhs.github.io/nemsqar/reference/safety_02_population.md),
  [`safety_04_population()`](https://bemts-hhs.github.io/nemsqar/reference/safety_04_population.md),
  [`seizure_02_population()`](https://bemts-hhs.github.io/nemsqar/reference/seizure_02_population.md)  
- [`stroke_01_population()`](https://bemts-hhs.github.io/nemsqar/reference/stroke_01_population.md),
  [`syncope_01_population()`](https://bemts-hhs.github.io/nemsqar/reference/syncope_01_population.md),
  [`tbi_01_population()`](https://bemts-hhs.github.io/nemsqar/reference/tbi_01_population.md)  
- [`trauma_01_population()`](https://bemts-hhs.github.io/nemsqar/reference/trauma_01_population.md),
  [`trauma_03_population()`](https://bemts-hhs.github.io/nemsqar/reference/trauma_03_population.md),
  [`trauma_04_population()`](https://bemts-hhs.github.io/nemsqar/reference/trauma_04_population.md)  
- [`trauma_08_population()`](https://bemts-hhs.github.io/nemsqar/reference/trauma_08_population.md),
  [`trauma_14_population()`](https://bemts-hhs.github.io/nemsqar/reference/trauma_14_population.md),
  [`ttr_01_population()`](https://bemts-hhs.github.io/nemsqar/reference/ttr_01_population.md)

##### Utility Functions

- [`results_summarize()`](https://bemts-hhs.github.io/nemsqar/reference/results_summarize.md),
  [`summarize_measure()`](https://bemts-hhs.github.io/nemsqar/reference/summarize_measure.md)
