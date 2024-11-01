################################################################################
### Respiratory-02 Function  ###################################################
################################################################################

###_____________________________________________________________________________
### Assume data are already loaded
### The data must be a dataframe or tibble that contains emedications.03 and eprocedures.03 as
### as columns where each cell contains all values entered for each respective incident.
### These are not list columns but can be comma separated values in each cell, and must contain
### all medications for each incident.  The table can include all values entered for evitals.12
### in the table.  This will cause row explosion, but the function will find if the patient had
### sp02 < 90 among any values entered.
### This function assumes you have the following fields:
### eresponse.05 (must contain code), evitals.12 (all values), emedications.03 (all with code),
### eprocedures.03 (with code)
### this function assumes that there is an age in years calculated for epatient.15
### this function also assumes that rows that are missing any value are NA,
### not the not known / not recorded values common to ImageTrend or the value codes
### that correspond to "not values".
### the first argument is a dataframe, no joining is done in the function.
### any needed table joins must be done prior to running the function to ensure
### required columns are present.
### grouping can be done before the function to get the calculations by region
### or other grouping
###_____________________________________________________________________________

respiratory_02 <- function(df,
                           eresponse_05_col,
                           evitals_12_col,
                           emedications_03_col,
                           eprocedures_03_col) {
  
  
  initial_population <- df %>% 
    filter(grepl(pattern = "2205001"))
  
  
}