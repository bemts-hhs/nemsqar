# Synthetic Test Data for eAirway Fields in National EMS Information System

This dataset provides completely synthetic test data for the
airway-related fields in the National EMS Information System (NEMSIS).
It is not specific to any single function but can be used to test
multiple functions in the `nemsqar` package. Users are encouraged to
experiment with this dataset, but results should not be interpreted as
meaningful. Some outputs may be nonsensical, which is expected since
this data is only intended to demonstrate the expected structure of
input data.

## Usage

``` r
data(nemsqar_airway_table)
```

## Format

A data frame with 10,000 rows and 8 variables:

- Incident Patient Care Report Number - PCR (eRecord.01):

  Unique identifier for the incident report (character).

- Incident Date:

  Date of the incident (Date).

- Airway Indications For Invasive Management List (eAirway.01):

  List of indications for invasive airway management (character).

- Airway Device Placement Confirmation Date Time (eAirway.02):

  Timestamp of airway device placement confirmation (datetime).

- Airway Device Being Confirmed (eAirway.03):

  Type of airway device being confirmed (character).

- Patient Airway Device Being Confirmed List (eAirway.03):

  List of airway devices being confirmed (character).

- Airway Device Placement Confirmed Method (eAirway.04):

  Primary method used to confirm airway device placement (character).

- Airway Device Placement Confirmed Method List (eAirway.04):

  List of methods used to confirm airway device placement (character).

## Examples

``` r
data(nemsqar_airway_table)
head(nemsqar_airway_table)
#> # A tibble: 6 × 8
#>   Incident Patient Care Report Number -…¹ `Incident Date` Airway Indications F…²
#>   <chr>                                   <date>          <chr>                 
#> 1 NyXFBlJfnm-8333586176                   2023-05-27      Adequate Airway Refle…
#> 2 XTLCINMLTP-8616021114                   2023-10-14      Apnea or Agonal Respi…
#> 3 HfYjlIEQSk-9529756610                   2023-07-07      Adequate Airway Refle…
#> 4 MOwVDhriyC-5915613206                   2023-05-13      Adequate Airway Refle…
#> 5 ZCGOtLEPKw-7820135532                   2023-11-24      Adequate Airway Refle…
#> 6 fEMvUCQCRQ-9052388486                   2023-01-12      Adequate Airway Refle…
#> # ℹ abbreviated names:
#> #   ¹​`Incident Patient Care Report Number - PCR (eRecord.01)`,
#> #   ²​`Airway Indications For Invasive Management List (eAirway.01)`
#> # ℹ 5 more variables:
#> #   `Airway Device Placement Confirmation Date Time (eAirway.02)` <dttm>,
#> #   `Airway Device Being Confirmed (eAirway.03)` <chr>,
#> #   `Patient Airway Device Being Confirmed List (eAirway.03)` <chr>, …
```
