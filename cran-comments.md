## R CMD check results

0 errors | 0 warnings | 0 note

* This is a minor release. 

## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

## Release Summary

This is a feature and maintenance release that introduces two new EMS
registry data quality measures (`data_01()` and `data_02()`), expands
population reporting across several existing measures, corrects multiple
measure-specification implementation issues, and improves package
documentation and internal performance.

Several changes in this release are intentionally breaking and may require
updates to downstream analysis scripts.

## Breaking Changes

1. `stroke_01_population()` now returns adult and pediatric population
   objects in addition to the initial population. Existing code that assumes
   a single population object may require modification.

2. `stroke_01()` now reports results for All, Adult, and Pediatric
   populations. Existing code that assumes a single-row result may require
   modification due to additional rows being returned.

3. `airway_01()`, `airway_05()`, `airway_18()`, `ttr_01()`,
   `trauma_14()`, `trauma_08()`, `trauma_04()`, and `syncope_01()`
   now report All, Adult, and Pediatric populations. Existing code that
   assumes only Adult and Pediatric outputs may require modification due to
   additional rows in returned results.

4. Multiple measure-specification corrections alter numerators and/or
   denominators and may therefore produce different results compared with
   previous releases:
   - `stroke_01_population()`
   - `asthma_01_population()`
   - `hypoglycemia_01_population()`
   - `safety_04_population()`
   - `trauma_04_population()`
   - `trauma_14_population()`
   - `ttr_01_population()`

5. `trauma_14_population()` now produces an explicit all-population
   denominator in addition to existing age-stratified denominator groups,
   enabling all-age Trauma-14 calculations.

## New Features

- Added `data_01_population()` and `data_01()` for EMS registry record
  validity score analyses.
- Added `data_02_population()` and `data_02()` for EMS registry
  data-entry timeliness analyses.

## Testing

- Package rebuilt and documented with current roxygen2 infrastructure.
- Unit tests added for the new `data_*` functions.
- Updated unit tests for functions mentioned above where updates took place.