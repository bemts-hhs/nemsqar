# Synthetic Test Data for eArrest Fields in National EMS Information System

This dataset provides completely synthetic test data for the cardiac
arrest-related fields in the National EMS Information System (NEMSIS).
It is not specific to any single function but can be used to test
multiple functions in the `nemsqar` package. Users are encouraged to
experiment with this dataset, but results should not be interpreted as
meaningful. Some outputs may be nonsensical, which is expected since
this data is only intended to demonstrate the expected structure of
input data.

## Usage

``` r
data(nemsqar_arrest_table)
```

## Format

A data frame with 10,000 rows and 28 variables:

- Incident Patient Care Report Number - PCR (eRecord.01):

  Unique identifier for the incident report (character).

- Incident Date:

  Date of the incident (Date).

- Cardiac Arrest During EMS Event With Code (eArrest.01):

  Indicates whether cardiac arrest occurred during the EMS event
  (character).

- Cardiac Arrest Etiology With Code (eArrest.02):

  Suspected cause of the cardiac arrest (character).

- Cardiac Arrest Indications Resuscitation Attempted By EMS
  (eArrest.03):

  Whether resuscitation was attempted by EMS (character).

- Cardiac Arrest Indications Resuscitation Attempted By EMS With Code
  List (eArrest.03):

  Detailed reasons for resuscitation decisions (character).

- Cardiac Arrest Witnessed By (eArrest.04):

  Who witnessed the cardiac arrest (character).

- Cardiac Arrest Witnessed By List (eArrest.04):

  List of all witnesses to the cardiac arrest (character).

- Cardiac Arrest CPR Provided Prior To EMS Arrival
  (3.4=eArrest.05/3.5=itArrest.105):

  Whether CPR was provided before EMS arrival (character).

- Cardiac Arrest Who Provided CPR Prior To EMS
  (3.4=eArrest.06/3.5=itArrest.106):

  Who performed CPR before EMS arrival (character).

- Cardiac Arrest Who Provided CPR Prior To EMS Arrival With Code List
  (3.4=eArrest.06/3.5=itArrest.106):

  List of responders who provided CPR before EMS arrival (character).

- Cardiac Arrest AED Use Prior To EMS Arrival (eArrest.07):

  Whether an AED was used before EMS arrival (character).

- Cardiac Arrest Who Used AED Prior To EMS
  (3.4=eArrest.08/3.5=itArrest.108):

  Who used the AED before EMS arrival (character).

- Cardiac Arrest Who Used AED Prior To EMS Arrival With Code List
  (3.4=eArrest.08/3.5=itArrest.108):

  List of responders who used the AED before EMS arrival (character).

- Cardiac Arrest Type Of CPR Provided List (eArrest.09):

  List of types of CPR performed (character).

- Cardiac Arrest First Monitored Arrest Rhythm Of Patient (eArrest.11):

  First recorded cardiac rhythm during arrest (character).

- Cardiac Arrest First Monitored Arrest Rhythm Of Patient With Code
  (eArrest.11):

  Coded representation of the first monitored cardiac rhythm
  (character).

- Cardiac Arrest Any Return Of Spontaneous Circulation (eArrest.12):

  Whether the patient regained spontaneous circulation (character).

- Cardiac Arrest Any Return Of Spontaneous Circulation With Code List
  (eArrest.12):

  List of codes indicating ROSC status (character).

- Cardiac Arrest Date Time (eArrest.14):

  Timestamp of cardiac arrest occurrence (datetime).

- Cardiac Arrest Resuscitation Discontinued Date Time (eArrest.15):

  Timestamp of resuscitation discontinuation (datetime).

- Cardiac Arrest Reason CPR Resuscitation Discontinued (eArrest.16):

  Reason for discontinuing resuscitation (character).

- Cardiac Arrest Rhythm On Arrival At Destination List (eArrest.17):

  Recorded cardiac rhythm upon arrival at the destination (character).

- Cardiac Arrest Patient Outcome At End Of EMS Event (eArrest.18):

  Patient's condition at the end of the EMS event (character).

- Cardiac Arrest Initial CPR Date Time (eArrest.19):

  Timestamp of initial CPR administration (datetime).

- Cardiac Arrest Who Initiated CPR With Code
  (3.4=itArrest.008/3.5=eArrest.20):

  Who initiated CPR (character).

- Cardiac Who First Applied The AED (3.4=itArrest.015/3.5=eArrest.21):

  Who first applied the AED (character).

- Cardiac Who First Defibrillated The Patient
  (3.4=itArrest.013/3.5=eArrest.22):

  Who performed the first defibrillation (character).

## Examples

``` r
data(nemsqar_arrest_table)
head(nemsqar_arrest_table)
#> # A tibble: 6 × 28
#>   Incident Patient Care Report Number -…¹ `Incident Date` Cardiac Arrest Durin…²
#>   <chr>                                   <date>          <chr>                 
#> 1 NyXFBlJfnm-8333586176                   2023-05-27      No (3001001)          
#> 2 XTLCINMLTP-8616021114                   2023-10-14      Yes, After EMS Arriva…
#> 3 HfYjlIEQSk-9529756610                   2023-07-07      No (3001001)          
#> 4 MOwVDhriyC-5915613206                   2023-05-13      Yes, Prior to EMS Arr…
#> 5 ZCGOtLEPKw-7820135532                   2023-11-24      No (3001001)          
#> 6 fEMvUCQCRQ-9052388486                   2023-01-12      Not Applicable (77010…
#> # ℹ abbreviated names:
#> #   ¹​`Incident Patient Care Report Number - PCR (eRecord.01)`,
#> #   ²​`Cardiac Arrest During EMS Event With Code (eArrest.01)`
#> # ℹ 25 more variables: `Cardiac Arrest Etiology With Code (eArrest.02)` <chr>,
#> #   `Cardiac Arrest Indications Resuscitation Attempted By EMS (eArrest.03)` <chr>,
#> #   `Cardiac Arrest Indications Resuscitation Attempted By EMS With Code List (eArrest.03)` <chr>,
#> #   `Cardiac Arrest Witnessed By (eArrest.04)` <chr>, …
```
