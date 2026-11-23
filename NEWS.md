# nemsqar 1.3.0

## Features

- `stroke_01_population()` now returns adult, pediatric, and initial
  population datasets consistent with other population-generating functions in
  `nemsqar`. This is a breaking change because existing scripts that assume
  only an initial population object is returned may require modification.

- `stroke_01()` now reports results for adult, pediatric, and overall
  populations consistent with other wrapper functions in `nemsqar`. This is a
  breaking change because existing scripts that assume only overall results are
  returned may require modification due to additional rows in the output.

- Quality-of-life improvements to `airway_01()`, `airway_05()`,
  `airway_18()`, `ttr_01()`, `trauma_14()`, `trauma_08()`, `trauma_04()`, and
  `syncope_01()` now report results for adults, pediatrics, and the overall
  denominator population as defined by the corresponding NEMSQA measure. These
  are breaking changes because existing scripts that assume only adult and
  pediatric results are returned may require modification due to additional
  rows in the output.

- Added `data_01_population()` and `data_01()` for analysis of EMS registry
  record validity scores.

- Added `data_02_population()` and `data_02()` for analysis of EMS registry
  data-entry timeliness.

- `data_01()` and `data_02()` are not official NEMSQA measures. They were
  developed to support EMS data quality assessment and quality improvement
  initiatives related to data validity, completeness, and timeliness within EMS
  registries.

- Updated `filter_process` output in `hypoglycemia_01_population()` and
  `respiratory_02_population()` to report the combined numerator pathways used
  by the respective measures. `hypoglycemia_01_population()` now summarizes
  hypoglycemia treatment delivered either through qualifying medications or
  procedures, and `respiratory_02_population()` now summarizes oxygen
  administration delivered either as a medication or oxygen therapy procedure,
  improving alignment between filter_process output and measure numerator
  definitions.

- Improved performance in `trauma_03_population()` by refactoring first/last
  pain score determination to use encounter-level summarization rather than
  grouped row-wise mutation. The function now derives initial and final pain
  scores and corresponding assessment times using a sorted `dplyr::summarize()`
  workflow, substantially reducing row expansion and memory utilization while
  preserving identical numerators, denominators, and calculated performance
  results. 

- Updated all `*_population()` functions to pass `calls = 2` to internal
  validation helpers (e.g., `validate_data_pull()`, `validate_numeric()`,
  `validate_class()`, and related validate functions). This change ensures that
  warnings, messages, and errors correctly reference the user-facing
  population function that triggered the validation rather than an internal
  helper function, resulting in more accurate and informative diagnostics.

## Documentation

- Corrected multiple comments in wrapper functions that incorrectly described
  the number of population groups analyzed. In several cases, comments
  referenced two groups despite results being calculated and displayed for
  three populations.

- Added clarification to each `*_population()` function about how adult and
  pediatric populations are identified within each function.

## Bug fixes

- `stroke_01_population()` incorrectly applied exclusion criteria for cases
  where GCS <= 9 or AVPU = "Unresponsive" patient records need to be excluded.
  It was using logic like `!GCS | !AVPU` instead  of `!(GCS | AVPU)` 
  (`!GCS & !AVPU` would work just as well). This logic creates a variable 
  `NOT_GCS_AVPU` which is a logical flag that is used to filter out those
  records to create the `initial_population` object for  `stroke_01()` to use.
  `stroke_01_population()` now uses `!(GCS | AVPU)`. This is a breaking change
  given that it will slightly decrease the total number of records analyzed in
  any given run of `stroke_01_population()` or `stroke_01()`, but performance is
  expected to remain similar.

- `asthma_01_population()` now correctly filters the pediatric population down
  to ages >= 2 years and age < 18 years when using calculated ages or
  system-defined ages from the `epatient_15` and `epatient_16` arguments.
  Previously, `asthma_01_population()` included all patients age < 18 yrs in the
  pediatric population without excluding patients age < 2 yrs.

- Fixed a denominator filtering error in `hypoglycemia_01_population()`.
  Previously, the 911 response criterion (eResponse.05) was only applied to
  records meeting the altered mental status and low blood glucose inclusion
  pathway due to logical operator precedence. The denominator now correctly
  requires a qualifying 911 response for records meeting either the
  diabetes/hypoglycemia with altered mental status pathway or the altered mental
  status with low blood glucose pathway. This is not expected to cause dramatic
  changes in results, but will be a breaking changes as results will be
  different as compared to previous versions.

- Fixed a denominator age-classification inconsistency in
  `safety_01_population()`. Previously, when `incident_date_col` and
  `patient_DOB_col` were not provided, adult age classification used a
  case-sensitive equality check on `epatient_16_col`, which could incorrectly
  exclude valid adult records when age units were recorded using alternative
  capitalization. Adult age classification now consistently uses the same
  case-insensitive age-unit matching logic applied throughout the function and
  throughout `nemsqar`. This is not expected to result in material changes to
  outputs as the equality used was `epatient_16 == "Years"`, which is what most
  EMS registries will use as it is the NEMSIS standard.

- Fixed pediatric age classification in `safety_04_population()` to align with
  the published Safety-04 denominator specification. Patients exactly 8 years of
  age are no longer included in the denominator, and age-unit classification for
  patients measured in days, hours, and minutes now uses the new
  days_hours_minutes regular expression defined in `constants.R`, improving
  adherence to the measure definition for patients less than 8 years of age.
  This is a breaking change but the impact is expected to be quite small as it
  only affects edge cases. 

- Corrected a labeling error in `stroke_01_population()` where the
  `filter_process` summary reported `GCUS <= 9`; the label now correctly reports
  `GCS <= 9`.  

- Fixed argument validation in `syncope_01_population()` to properly require
  age-related column arguments used for denominator stratification. Previously,
  `epatient_15_col`, `epatient_16_col`, `incident_date_col`, and
  `patient_DOB_col` were not consistently checked during argument validation.
  The function now correctly validates all required age and date inputs before
  population construction. 

- Fixed argument validation in `tbi_01_population()` to properly require age-
  and date-related column arguments used in denominator stratification.
  Previously, `epatient_15_col`, `epatient_16_col`, `incident_date_col`, and
  `patient_DOB_col` were not consistently validated before age-group
  calculations were performed. The function now validates all required age and
  date inputs prior to population construction. Adult age classification was
  also updated to use the package-wide `year_values` regular expression with
  case-insensitive matching rather than relying on a static "Years" comparison,
  improving consistency with age logic used throughout `nemsqar`. 

- In `trauma_04_population()`, the pediatric (`< 10 years`) systolic blood
  pressure field triage criterion is now correctly evaluated as 
  `SBP >= (70 + (Age × 2))`, consistent with the published Trauma-04
  specification. 

- Fixed pediatric field triage logic in `trauma_14_population()` to correctly
  evaluate age-adjusted systolic blood pressure criteria for patients younger
  than 10 years of age. The pediatric SBP threshold now uses 
  `SBP >= (70 + (Age × 2))`, aligning the implementation with the published
  Trauma-14 specification. In addition, an explicit total_population object was
  added by combining the mutually exclusive age-stratified denominator
  populations (`<10`, `10–64`, and `65+` years), enabling calculation of all-age
  Trauma-14 results while preserving the existing age-specific denominator
  logic. 

- Fixed numerator eligibility logic in `ttr_01_population()` by correcting AVPU
  documentation detection to use `evitals_26_col` (AVPU) rather than
  `evitals_23_col` (GCS). Previously, the numerator check effectively evaluated
  GCS documentation twice and did not independently assess AVPU documentation.
  The update aligns the implementation with the TTR-01 specification requiring
  documentation of SBP, DBP, heart rate, pulse oximetry, respiratory rate, and
  either GCS or AVPU.

- All `*_population()` functions now save and restore the user's
  `cli.progress_bar_style` option after execution. This prevents package
  functions from leaving persistent changes to global session options and
  improves package hygiene when using custom progress bars.

# nemsqar 1.2.1
- Updated how wrapper functions report runtime (e.g., `airway_01()`). Runtime is
  now calculated using `difftime(units = "auto")`, converting the result with
  `as.numeric()` and extracting the dynamic time unit via 
  `attr(which = "units")`. This makes the time unit automatic and avoids
  potential downstream issues.  

- Corrected a typo involving ETCO2 across several functions, `globalVariables.R`,
  and `constants.R`, where it appeared as “etc02” in various cases. All instances
  were fixed by replacing the zero with the letter “o” (case‑sensitive). 

# nemsqar 1.2.0
- Update `trauma_04_population()` to look to the facility ID instead of the
  hospital capabilities for performance measurement. This is a breaking change
  but will make the measure more accurate. This change comes as part of the
  NEMSQA Measure Development committee has discussed this action seeing dismal
  national performance on this measure.
- Added the new function `nemsqa_missing_summary()` to summarize missing data
  within each `*_popoulation()` function and to be included in the `list` object
  returned by the same functions.
- Added the first `nemsqar` vignette! Please use `vignette(topic = "nemsqar",
  package = "nemsqar")` to check it out.
- Added tests for `validate_set()`, `validate_numeric()` tests were improved,
  `validate_names()`, added tests for `validate_length()` and
  `compare_formals()`. 
- Removed documentation for the `validate_*()` family of functions and use
  keyword internal in their documentation. 
- `results_summarize()` and `summarize_measure()` no longer have external facing
  documentation and are internal. 
- Added a contributor's code of conduct.
- Removed some language giving thanks to early contributors from the README file
  and placed that in the initial release of `nemsqar`.
- Implemented `usethis::use_tidy_*` package helpers to clean up various parts of
  the package such as workflows and using `Air` as the code formatter
  throughout. 

Thanks to [&#x0040;bemts-hhs](https://github.com/bemts-hhs) (maintenance and
programming in this release),
[&#x0040;eliza-little](https://github.com/eliza-little) (for submitting issue
[#4](https://github.com/bemts-hhs/nemsqar/issues/4)), and
[&#x0040;samuelkordik](https://github.com/samuelkordik) (submitted issue
[#15](https://github.com/bemts-hhs/nemsqar/issues/15) and reviewed PR
[#23](https://github.com/bemts-hhs/nemsqar/pull/23)) for all your contributions
to this CRAN release!

# nemsqar 1.1.4
* Documentation using @inheritParams to help with making functions more
  maintainable.
* Update regex within each function so they utilize paste() to make the regex
  more maintainable.
* Created `constants.R` to contain all regex for the package as internal to the
  package. `constants.R` is loaded at package install / load and these regex
  objects are available to the package for the functions to reference.
* Put regex (be sensitive to measure-specific regex) into .R/constants.R file to
  centralize these and functions just reference these objects. Then, we can
  delete all regex within functions for an even easier time maintaining the
  package. This addresses issue #22.
* Update data validation where applicable to use functional validation.
* Abbreviate the code base by removing separate workflows for the 'df' and
  'table' data sources. Unify these by always ending up with 'tables' by
  splitting up a 'df' if supplied, and then only needing to proceed with major
  data validation and the analysis on objects named like `*_table`. 
* Replace data validation that involve rlang::as_name(quo) with making this code
  more readable by creating a separate `*_name` object for applicable columns
  and then running validation on `object[[name]]`.

# nemsqar 1.1.3
- Added navigation to all major functions in the package. This was done by
  adding `----` to the end of most comments.
- Some functions received code formatting due to the addition of the `----` or
  just by virtue of editing the script, the `Air` package does the formatting
  via Positron.
- Improved comments in the code in some functions and removed one code
  redundancy in a function that performed the exact same check that the
  `*_table` and `df` arguments were receiving inputs simulataneously.

# nemsqar 1.1.2

- Fixed broken URLs in the documentation for `nemsqa_binomial_confint()`.

# nemsqar 1.1.1

- In `airway_01_population()`, the filter_process object had one text descriptor `"All initial population successful intubation with no hypoxia or hypoxia/hypotension"`
which was corrected to be `"All initial population successful intubation with no hypoxia/hypotension"`.

- Also, in `trauma_14_population()`, the `filter_process` object has one text descriptor `"Tournique procedure"` which was corrected to be `"Tourniquet procedure"`.

- Additionally, trauma_14_population() should *just work* after passing an arbitrary number of (applicable) columns to the `transport_disposition` argument. In {nemsqar} 1.1.0, only one column will work as the `grepl()` call is not wrapped in `dpyr::if_any()`. This was fixed so that users can reference other columns that contain transport disposition data, such as eDisposition.12 from NEMSIS 3.4 (or earlier versions as applicable). This helps with back compatibility when using this measure to earlier versions of NEMSIS, or later versions.

# nemsqar 1.1.0  

## Enhancements  

- **Optional Confidence Intervals**: Added the ability to compute confidence intervals using the Wilson or Clopper-Pearson (exact) method. This feature is optional and can be enabled when working with sample data.
  - Introduced `nemsqa_binomial_confint()`, a lightweight wrapper around `prop.test()` and `binom.test()` for calculating Wilson and exact confidence intervals. This function eliminates the need for an additional package dependency.
    * Ensure warning messages where any `denominator` < 10 are elegant and helpful, and `nemsqa_binomial_confit()` handles division by zero cases well.
  - Updated all wrapper functions (e.g., `airway_01()`) to support optional confidence interval calculation.
  - Maintained full backward compatibility with **nemsqar 1.0.0** by setting `confidence_interval = FALSE` as the default behavior.
- **Dynamic `results_summarize()`**: Enhanced `results_summarize()` to dynamically calculate only the specified groups, utilizing the previously unused `population_labels` object. This reduces unnecessary calculations and streamlines function performance.  
- **Improved Documentation**: 
  - Updated and expanded the documentation for `results_summarize()` and `summarize_measure()`, offering clearer usage instructions and examples to enhance the user experience.
  - Refined the documentation for multiple other functions, improving clarity and usability.

# nemsqar 1.0.0  

## Initial CRAN Release  

- First official submission of **nemsqar** to CRAN.  

# nemsqar 0.1.0  

## Package Inception  

- **nemsqar is born!** This initial version laid the foundation for calculating National EMS Quality Alliance (NEMSQA) performance measures in a structured and modular way.  

### Key Features  

- Designed core functions to **identify target populations** and **compute performance measures** for EMS quality metrics.  
- Implemented a modular structure for measure calculations, with `_population` workhorse functions handling data extraction and `measure_##` wrapper functions streamlining performance calculations.  
- Developed functions to align with **NEMSQA measure technical documents**.  

### Implemented Functions  

#### Measure Functions  
- `airway_01()`, `airway_05()`, `airway_18()`  
- `asthma_01()`, `hypoglycemia_01()`, `pediatrics_03b()`  
- `respiratory_01()`, `respiratory_02()`, `safety_01()`  
- `safety_02()`, `safety_04()`, `seizure_02()`  
- `stroke_01()`, `syncope_01()`, `tbi_01()`  
- `trauma_01()`, `trauma_03()`, `trauma_04()`  
- `trauma_08()`, `trauma_14()`, `ttr_01()`  

#### Population Functions  
- `airway_01_population()`, `airway_05_population()`, `airway_18_population()`  
- `asthma_01_population()`, `hypoglycemia_01_population()`, `pediatrics_03b_population()`  
- `respiratory_01_population()`, `respiratory_02_population()`, `safety_01_population()`  
- `safety_02_population()`, `safety_04_population()`, `seizure_02_population()`  
- `stroke_01_population()`, `syncope_01_population()`, `tbi_01_population()`  
- `trauma_01_population()`, `trauma_03_population()`, `trauma_04_population()`  
- `trauma_08_population()`, `trauma_14_population()`, `ttr_01_population()`  

#### Utility Functions  
- `results_summarize()`, `summarize_measure()`

## Thanks!

A big loud thanks to Sheree Murphy from NEMSQA, Peter Geissert from Oregon EMS & Trauma Systems, Jeffrey Jarvis medical director for the Metropolitan Area EMS Authority (Fort Worth, Texas), and Marshall Washick (Washington D.C. Fire) for all their input and consultation!
