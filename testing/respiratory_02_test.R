# load data

respiratory_02_data <- read_csv("C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/EMS DATA FOR ALL SCRIPTS/NEMSQA/respiratory02_Export.csv")

# get the initial population - first filter pass
# create a Unique_ID to get a better estimate of N
respiratory_02_prep <- respiratory_02_data  %>%
  mutate(across(c(`Patient Date Of Birth (ePatient.17)`, `Incident Date`), ~ mdy(
    str_remove_all(string = ., pattern = "\\s12:00:00\\sAM")
  )))

# test the function

respiratory_02_prep %>% 
  respiratory_02(erecord_01_col = `Incident Patient Care Report Number - PCR (eRecord.01)`,
                 incident_date_col = `Incident Date`,
                 patient_DOB_col = `Patient Date Of Birth (ePatient.17)`,
                 eresponse_05_col = `Response Type Of Service Requested With Code (eResponse.05)`,
                 evitals_12_col = `Vitals Pulse Oximetry (eVitals.12)`,
                 emedications_03_col = `Patient Medication Given or Administered RXCUI Codes List (eMedications.03)`,
                 eprocedures_03_col = `Patient Attempted Procedure Codes List (eProcedures.03)`
                 )
