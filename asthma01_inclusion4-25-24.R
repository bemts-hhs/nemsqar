############################################
##
##  
##          NEMSQA ASTHMA-01              ##
##
##
############################################

## Description
## This function takes in a dataset with indicators for performance measure 
## inclusion in the numerator and denominators for all Populations. Formats the 
## datetime field and outputs integer year, month and quarter columns. The 
## summarizes to generate a table with numerator and denomenator counts for 
## each of three denominators, Pediatric, Adult and Total.

## Requires
# library(tidyverse)

## Input
## Takes two dfs that must contain the following columns:
## nemsqa_inclusion1
  ## Incident.ID..itRecord.001., 
  ## Patient.Age.In.Years..ePatient.15.,
  ## Situation.Provider.Primary.Impression.Code..eSituation.11.,
  ## Situation.Provider.Secondary.Impression.Description.And.Code.List..eSituation.12.    
  ## Incident.Unit.Notified.By.Dispatch.Date.Time..eTimes.03.                             datetime
  ## ped_ind,                                                                             0/1
  ## adult_ind                                                                            0/1
  ## tot_ind                                                                              0/1
## nemsqa_meds
  ## Incident.ID..itRecord.001., 
  ## Medication.Given.Description..eMedications.03.   

## Output
## This function will output a df with the following columns:
  ## year  
  ## quarter
  ## month         
  ## tot_denom    
  ## ped_denom    
  ## adult_denom   
  ## tot_num      
  ## ped_num      
  ## adult_num    

## Arguments
  ## df - Name of the input dataframe

## Change Log
  ## Created 3.11.24 Peter Geissert

## Documented Issues
  ## 


## Beta Agonists:
# eMedication.03 Medication Given is in
# (435 ("Albuterol"),
#   7688 ("metaproterenol"),
#   214199 ("Albuterol/Ipratropium"),
#   237159 ("Levalbuterol"),
#   487066 ("levalbuterol tartrate"),
#   1154062 ("Albuterol Inhalant Product"),
#   1163444 ("Levalbuterol Inhalant Product"),
#   1649559 ("Albuterol Dry Powder Inhaler"),
#   1165719 ("metaproterenol Inhalant Product"),
#   2108209 ("Levalbuterol Inhalation Solution"),
#   2108252 ("metaproterenol Inhalation Solution")

# Asthma Diagnosis codes:
# eSituation.11 Provider's Primary Impression
# eSituation.12 Provider's Secondary Impressions
# J45     "Asthma"
# J98.01  "Acute bronchospasm")

## NEMSQA Asthma-01 Denominator
# ePatient.15 Age is greater than or equal to 2
# and	ePatient.15 Age is less than or equal to 18
# and	eResponse.05 Type of Service Requested is 2205001 ("911 Response (Scene)")

asthma01_inclusion <- function(df) {
  
  beta_agonist <- c("ALBUTEROL", "IPRATROPIUM", "DUONEB", "LEVALBUTEROL", "METAPROTERENOL")
 
  asthma_codes <- c("J45", "J98.01")
  
  df %>%
    select(., key, 
           ePatient.15,
           eSituation.11,
           eSituation.12,
           eTimes.03,
           ped_ind,
           adult_ind,
           tot_ind, 
           year, 
           quarter, 
           month) %>%
    
    ## NEMSQA Asthma-01 Denominator
    mutate(., ped_ind = case_when((ped_ind == 1 & ePatient.15 >= 2) ~ 1, 
                                  TRUE ~ 0)) %>% 
    filter(.,  grepl(orify(asthma_codes), eSituation.11)|
             grepl(orify(asthma_codes), eSituation.12)) %>%
    
    ## NEMSQA Asthma-01 Numerator
    left_join(., nemsqa_meds %>%
                filter(., grepl(orify(beta_agonist), toupper(eMedications.03))) %>%
                distinct(., key) %>%
                mutate(., num_ind = 1), 
              by = c("key" = "key")) %>%
    mutate(., num_ind = case_when(is.na(num_ind) ~ 0, 
                                  TRUE ~ num_ind))
  
}
