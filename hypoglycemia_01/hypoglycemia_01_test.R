################################################################################
### Test for Hypoglycemia-01 ###################################################
################################################################################

# load data

hypoglycemia_01_data <- read_csv("C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/EMS DATA FOR ALL SCRIPTS/NEMSQA/hypoglycemia01_Export.csv") %>% 
  clean_names(case = "screaming_snake", sep_out = "_")

# clean data

hypoglycemia_01_clean <- hypoglycemia_01_data %>% 
  mutate(across(c(INCIDENT_DATE, PATIENT_DATE_OF_BIRTH_E_PATIENT_17), ~  mdy(str_remove_all(., pattern = "\\s12:00:00\\sAM"))
                )
         )

# run the function

hypoglycemia_01_clean %>% 
  hypoglycemia_01(erecord_01_col = INCIDENT_PATIENT_CARE_REPORT_NUMBER_PCR_E_RECORD_01,
                  incident_date_col = INCIDENT_DATE,
                  patient_DOB_col = PATIENT_DATE_OF_BIRTH_E_PATIENT_17,
                  eresponse_05_col = RESPONSE_TYPE_OF_SERVICE_REQUESTED_WITH_CODE_E_RESPONSE_05,
                  esituation_11_col = SITUATION_PROVIDER_PRIMARY_IMPRESSION_CODE_AND_DESCRIPTION_E_SITUATION_11,
                  esituation_12_col = SITUATION_PROVIDER_SECONDARY_IMPRESSION_DESCRIPTION_AND_CODE_LIST_E_SITUATION_12,
                  evitals_18_col = VITALS_BLOOD_GLUCOSE_LEVEL_E_VITALS_18,
                  evitals_23_cl = VITALS_TOTAL_GLASGOW_COMA_SCORE_GCS_E_VITALS_23,
                  evitals_26_col = VITALS_LEVEL_OF_RESPONSIVENESS_AVPU_E_VITALS_26,
                  emedications_03_col = PATIENT_MEDICATION_GIVEN_OR_ADMINISTERED_DESCRIPTION_AND_RXCUI_CODES_LIST_E_MEDICATIONS_03
                  )
