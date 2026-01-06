# Synthetic Test Data for eInjury Fields in National EMS Information System

This dataset provides completely synthetic test data for evaluating
injury-related functions in the `nemsqar` package. It includes key
variables related to the cause of injury, trauma triage criteria,
vehicular risk factors, and height of falls. The dataset is intended to
assist users in testing the expected input structure for injury-related
measures.

## Usage

``` r
data(nemsqar_injury_table)
```

## Format

A data frame with 10,000 rows and 8 variables:

- Incident Patient Care Report Number - PCR (eRecord.01):

  Unique identifier for the incident report (character).

- Incident Date:

  Date of the incident (Date).

- Injury Cause of Injury (eInjury.01):

  General description of the cause of injury (character).

- Injury Cause Of Injury Description And Code List (eInjury.01):

  Detailed description and coding of injury causes (character).

- Injury Trauma Center/Triage Criteria (Steps 1 and 2) List
  (eInjury.03):

  List of trauma triage criteria met in Steps 1 and 2 (character).

- Injury Vehicular Pedestrian Or Other Injury Risk Factor/Triage
  Criteria (Steps 3 and 4) (eInjury.04):

  Primary vehicular or other risk factors for injury (character).

- Injury Vehicular Pedestrian Or Other Injury Risk Factor/Triage
  Criteria (Steps 3 and 4) List (eInjury.04):

  Detailed list of vehicular or pedestrian injury risk factors
  (character).

- Injury Height Of Fall In Feet (eInjury.09):

  Height of fall in feet when applicable (numeric).

## Details

Users are encouraged to test these functions with this dataset, but
results should not be interpreted as meaningful. Some outputs may be
nonsensical, which is expected since this data is only intended to
demonstrate the expected structure of input data.

## Examples

``` r
data(nemsqar_injury_table)
head(nemsqar_injury_table)
#> # A tibble: 6 × 8
#>   Incident Patient Care Report Number -…¹ `Incident Date` Injury Cause of Inju…²
#>   <chr>                                   <date>          <chr>                 
#> 1 NyXFBlJfnm-8333586176                   2023-05-27      Other and unspecified…
#> 2 XTLCINMLTP-8616021114                   2023-10-14      Stabbed/Cut by sharp …
#> 3 HfYjlIEQSk-9529756610                   2023-07-07      Electrocution from el…
#> 4 MOwVDhriyC-5915613206                   2023-05-13      Motorcycle driver inj…
#> 5 ZCGOtLEPKw-7820135532                   2023-11-24      Parachutist accident  
#> 6 fEMvUCQCRQ-9052388486                   2023-01-12      Fall from other furni…
#> # ℹ abbreviated names:
#> #   ¹​`Incident Patient Care Report Number - PCR (eRecord.01)`,
#> #   ²​`Injury Cause of Injury (eInjury.01)`
#> # ℹ 5 more variables:
#> #   `Injury Cause Of Injury Description And Code List (eInjury.01)` <chr>,
#> #   `Injury Trauma Center/Triage Criteria (Steps 1 and 2) List (eInjury.03)` <chr>,
#> #   `Injury Vehicular Pedestrian Or Other Injury Risk Factor/Triage Criteria (Steps 3 and 4) (eInjury.04)` <chr>, …
```
