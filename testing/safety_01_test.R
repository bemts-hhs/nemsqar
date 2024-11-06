################################################################################
### Test for Safety-01 #########################################################
################################################################################

# load data

safety_01_data <- read_csv("C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/EMS DATA FOR ALL SCRIPTS/NEMSQA/safety01_02_Export.csv") %>% 
  clean_names(case = "screaming_snake", sep_out = "_")

# clean

safety_01_clean <- safety_01_data %>% 
  mutate(across(c(INCIDENT_DATE, PATIENT_DATE_OF_BIRTH_E_PATIENT_17), ~ mdy(
    str_remove_all(., pattern = "\\s12:00:00\\sAM")
  )))

# run function

safety_01_clean %>% 
  safety_01(incident_date_col = INCIDENT_DATE,
            patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
            eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
            eresponse_24_col = RESPONSE_ADDITIONAL_RESPONSE_MODE_DESCRIPTORS_LIST_E_RESPONSE_24
            )
