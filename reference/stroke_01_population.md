# Stroke-01 Populations

Filters data down to the target populations for Stroke-01, and
categorizes records to identify needed information for the calculations.

Identifies key categories related to stroke-related incidents in an EMS
dataset, specifically focusing on cases where 911 was called for stroke,
and a stroke scale was administered. .

## Usage

``` r
stroke_01_population(
  df = NULL,
  patient_scene_table = NULL,
  response_table = NULL,
  situation_table = NULL,
  vitals_table = NULL,
  erecord_01_col,
  eresponse_05_col,
  esituation_11_col,
  esituation_12_col,
  evitals_23_col,
  evitals_26_col,
  evitals_29_col,
  evitals_30_col
)
```

## Arguments

- df:

  A data frame or tibble containing the dataset. Each row should
  represent a unique patient encounter.

- patient_scene_table:

  A data frame or tibble containing only epatient and escene fields as a
  fact table. Default is `NULL`.

- response_table:

  A data frame or tibble containing only the eresponse fields needed for
  this measure's calculations. Default is `NULL`.

- situation_table:

  A data.frame or tibble containing only the esituation fields needed
  for this measure's calculations. Default is `NULL`.

- vitals_table:

  A data.frame or tibble containing only the evitals fields needed for
  this measure's calculations. Default is `NULL`.

- erecord_01_col:

  The column containing unique record identifiers for each encounter.

- eresponse_05_col:

  The column containing EMS response codes, which should include 911
  response codes.

- esituation_11_col:

  The column containing the primary impression codes or descriptions
  related to the situation.

- esituation_12_col:

  The column containing secondary impression codes or descriptions
  related to the situation.

- evitals_23_col:

  The column containing the Glasgow Coma Scale (GCS) score.

- evitals_26_col:

  The column containing the AVPU (alert, verbal, pain, unresponsive)
  scale value.

- evitals_29_col:

  The column containing the stroke scale score achieved during
  assessment.

- evitals_30_col:

  The column containing stroke scale type descriptors (e.g., FAST, NIH,
  etc.).

## Value

A list that contains the following:

- a tibble with counts for each filtering step,

- a tibble for each population of interest

- a tibble for the initial population

- a tibble for the total dataset with computations

## Author

Nicolas Foss, Ed.D., MS

## Examples

``` r
# create tables to test correct functioning

  # patient table
  patient_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    incident_date = as.Date(c("2025-01-01", "2025-01-05",
                              "2025-02-01", "2025-01-01",
                              "2025-06-01")
                              ),
    patient_dob = as.Date(c("2000-01-01", "2020-01-01",
                            "2023-02-01", "2023-01-01",
                            "1970-06-01")
                            ),
    epatient_15 = c(25, 5, 2, 2, 55),  # Ages
    epatient_16 = c("Years", "Years", "Years", "Years", "Years")

  )

  # response table
  response_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    eresponse_05 = rep(2205001, 5)

  )

  # situation table
  situation_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    esituation_11 = c(rep("I60", 3), rep("I61", 2)),
    esituation_12 = c(rep("I63", 2), rep("I64", 3)),
  )

  # vitals table
  vitals_table <- tibble::tibble(

    erecord_01 = c("R1", "R2", "R3", "R4", "R5"),
    evitals_23 = c(16, 15, 14, 13, 12),
    evitals_26 = c("Alert", "Painful", "Verbal", "Unresponsive", "Alert"),
    evitals_29 = rep("positive", 5),
    evitals_30 = rep("a pain scale", 5)
  )

  # test the success of the function
  result <- stroke_01_population(patient_scene_table = patient_table,
                              response_table = response_table,
                              situation_table = situation_table,
                              vitals_table = vitals_table,
                              erecord_01_col = erecord_01,
                              eresponse_05_col = eresponse_05,
                              esituation_11_col = esituation_11,
                              esituation_12_col = esituation_12,
                              evitals_29_col = evitals_29,
                              evitals_23_col = evitals_23,
                              evitals_26_col = evitals_26,
                              evitals_30_col = evitals_30
                              )
#> Running `stroke_01_population()`  [Working on 1 of 11 tasks] ●●●●──────────────…
#> Running `stroke_01_population()`  [Working on 2 of 11 tasks] ●●●●●●────────────…
#> Running `stroke_01_population()`  [Working on 3 of 11 tasks] ●●●●●●●●●─────────…
#> Running `stroke_01_population()`  [Working on 4 of 11 tasks] ●●●●●●●●●●●●──────…
#> Running `stroke_01_population()`  [Working on 5 of 11 tasks] ●●●●●●●●●●●●●●●───…
#> Running `stroke_01_population()`  [Working on 6 of 11 tasks] ●●●●●●●●●●●●●●●●●─…
#> Running `stroke_01_population()`  [Working on 7 of 11 tasks] ●●●●●●●●●●●●●●●●●●…
#> Running `stroke_01_population()`  [Working on 8 of 11 tasks] ●●●●●●●●●●●●●●●●●●…
#> Running `stroke_01_population()`  [Working on 9 of 11 tasks] ●●●●●●●●●●●●●●●●●●…
#> Running `stroke_01_population()`  [Working on 10 of 11 tasks] ●●●●●●●●●●●●●●●●●…
#> Running `stroke_01_population()`  [Working on 11 of 11 tasks] ●●●●●●●●●●●●●●●●●…
#> 

# show the results of filtering at each step
result$filter_process
#> # A tibble: 7 × 2
#>   filter                              count
#>   <chr>                               <int>
#> 1 911 calls                               5
#> 2 Stroke cases                            5
#> 3 GCUS <= 9                               0
#> 4 AVPU = Unresponsive                     1
#> 5 Non-Null Stroke Scale Score or Type     5
#> 6 Initial population                      5
#> 7 Total dataset                           5
```
