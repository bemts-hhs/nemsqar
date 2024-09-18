############################################
##
##  
##      Generate aggregate Table        ##
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
## Takes a df that must contain the following columns:
  ## Incident.Unit.Notified.By.Dispatch.Date.Time..eTimes.03.   datetime
  ## tot_ind                                                    0/1
  ## ped_ind                                                    0/1
  ## adult_ind                                                  0/1
  ## num_ind                                                    0/1

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

## Issues
  ## 


############################################
##
##  
##      Generate aggregate Table        ##
##
##
############################################

performance_measure_agregate <- function(df) {
  
  df %>%
    
    group_by(year, quarter, month) %>% 
    dplyr::summarize(tot_denom = sum(tot_ind, na.rm = TRUE), 
                     ped_denom = sum(ped_ind, na.rm = TRUE),
                     adult_denom = sum(adult_ind, na.rm = TRUE),
                     tot_num = sum(num_ind, na.rm = TRUE), 
                     ped_num = sum(ped_ind * num_ind, na.rm = TRUE),
                     adult_num = sum(adult_ind * num_ind, na.rm = TRUE)) %>%
    ungroup() 
  
}