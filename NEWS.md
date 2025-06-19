# nemsqar 1.1.1

- In airway_01_population(), the filter_process object had one text descriptor "All initial population successful intubation with no hypoxia or hypoxia/hypotension"
which was corrected to be "All initial population successful intubation with no hypoxia/hypotension".

- Also, in trauma_14_population(), the filter_process object has one text descriptor "Tournique procedure" which was corrected to be "Tourniquet procedure".

- Additionally, trauma_14_population() should *just work* after passing an arbitrary number of (applicable) columns to the transport_disposition argument. In {nemsqar} 1.1.0, only one column will work as the `grepl()` call is not wrapped in `dpyr::if_any()`. This was fixed so that users can reference other columns that contain transport disposition data, such as eDisposition.12 from NEMSIS 3.4 (or earlier versions as applicable). This helps with back compatibility when using this measure to earlier versions of NEMSIS, or later versions.

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
