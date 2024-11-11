###_____________________________________________________________________________
# Function to clean up percent() formatted numbers
###_____________________________________________________________________________

pretty_percent <- function(variable, n_decimal = 0.1) {
  
  formatted_percent <- percent(variable, accuracy = n_decimal)
  
  # If there are trailing zeros after decimal point, remove them
  formatted_percent <- sub("(\\.\\d*?)0+%$", "\\1%", formatted_percent)
  
  # If it ends with ".%", replace it with "%"
  formatted_percent <- sub("\\.%$", "%", formatted_percent)
  
  formatted_percent
  
}
