################################################################################
### Respiratory_01 Function  ##########################################################
################################################################################

###_____________________________________________________________________________
### Assume data are already loaded
### Need to be a table where each row is 1 observation and each column is a feature
### or distinct datasets that can be referenced as unique columns
### this function assumes that there is an age in years calculated for epatient_15
### this function also assumes that rows that are missing any value are NA,
### not the not known / not recorded values common to ImageTrend or the value codes
### that correspond to "not values".
### the function assumes that vitals in the vital signs columns are likely the
### first vital signs, or are a list column.  This will give an indication of whether
### or not any vitals were taken.
### the esituation_12 is best as a list column of the secondary impressions entered
### the first argument is a dataframe, no joining is done.
### any joins to get vitals etc. will need to be done outside the function
### grouping can be done before the function to get the calculations by region
### or other grouping
###_____________________________________________________________________________


respiratory_01 <- function(df, eresponse_05_col, esituation_11_col, esituation_12_col, evitals_12_col, evitals_14_col, epatient_15_col) {
  
  # Filter incident data for 911 response codes and the corresponding primary/secondary impressions
  
  # 911 codes for eresponse.05
  codes_911 <- "2205001|2205003|2205009"
  
  # get codes as a regex to filter primary impression fields
  resp_codes <- "I50.9|J00|J05|J18.9|J20.9|J44.1|J45.901|J80|J81|J93.9|J96|J98.01|J98.9|R05|R06|R09.2|T17.9"
  
  # get codes that indicate a missing data element
  
  missing_codes <- "7701003|7701001"
  
  # filter the table to get the initial population regardless of age
  initial_population <- df %>% 
    
    # filter down to 911 calls
    
    dplyr::filter(grepl(pattern = codes_911, x = {{eresponse_05_col}}, ignore.case = T),
                  
                  # Identify Records that have Respiratory Distress Codes defined above
                  
                  if_any(
                    c({{esituation_11_col}}, {{esituation_12_col}}), ~ grepl(pattern = resp_codes, x = ., ignore.case = T)
                    )
                  ) %>% 
    
    # make sure that 
    mutate(vitals_check = if_else(!is.na({{evitals_12_col}}) & !is.na({{evitals_14_col}}), 1, 0
                                  )
           )
  
  # Adult and Pediatric Populations
  
  # filter adult
  adult_pop <- initial_population %>% 
    dplyr::filter({{epatient_15_col}} >= 18)
  
  # filter peds
  peds_pop <- initial_population %>% 
    dplyr::filter({{epatient_15_col}} < 18)
  
  # get the summary of results
  
  # all
  total_population <- initial_population %>% 
    summarize(pop = "All",
              numerator = sum(vitals_check, na.rm = T),
              denominator = n(),
              prop = numerator / denominator,
              prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01)
    )
  
  # adults
  adult_population <- adult_pop %>% 
    summarize(pop = "Adults",
              numerator = sum(vitals_check, na.rm = T),
              denominator = n(),
              prop = numerator / denominator,
              prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01)
    )
  
  # peds
  peds_population <- peds_pop %>% 
    summarize(pop = "Peds",
              numerator = sum(vitals_check, na.rm = T),
              denominator = n(),
              prop = numerator / denominator,
              prop_label = pretty_percent(numerator / denominator, n_decimal = 0.01)
    )
  
  # summary
  resp_01 <- bind_rows(adult_population, peds_population, total_population)
  
  resp_01
  
  
}
