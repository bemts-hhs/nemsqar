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
  data validation and the analysis once no matter what. 
* Replace data validation that involve rlang::as_name(quo) with making this code
  more readable by creating a separate `*_name` object for applicable columns
  and then running validation on `object[[name]]`. You need to do this across
  all functions, make sure to check all `*_population()` functions in the
  package to ensure this method is used consistently. 

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
