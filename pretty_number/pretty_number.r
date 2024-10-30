###_____________________________________________________________________________
# pretty_number() function
###_____________________________________________________________________________

pretty_number <- function(x, n_decimal = 2, prefix = NULL, round = T) {
  
  if(!"tidyverse" %in% installed.packages()) {
    
    install.packages("tidyverse")
    
  } else if(!"package:tidyverse" %in% search()) {
    
    library(tidyverse)
    
  }
  
  scipen_val <- options()$scipen # get current scipen setting
  
  options(scipen = 9999) # set scipen to allow very long numbers
  
  # set values to different orders of magnitude
  thousand <- 1e3
  million <- 1e6
  billion <- 1e9
  trillion <- 1e12
  quadrillion <- 1e15
  quintillion <- 1e18
  sextillion <- 1e21
  septillion <- 1e24
  octillion <- 1e27
  nonillion <- 1e30
  decillion <- 1e33
  
  if(round == F) { # without rounding, you can get numbers like 0.6k, so you can get the next period up
                   # it is important when using round == F to have numbers that are rounded to no further than 1 significant digit
  
  # get the number of characters in x
  x_length <- nchar(x)
  
  } else {
    
  rounded_x <- round(x)
  
  # get the number of characters in x
  x_length <- nchar(rounded_x)
    
  }
  
  # classify x
  
  suffix <- c("k", "m", "b", "t", "qd", "qt", "sxt", "spt", "oct", "non", "dec")
  
    
  x_val <- case_when(x_length %in% 4:6 ~ paste0(round(x / thousand, digits = n_decimal), suffix[1]),
                       x_length %in% 7:9 ~ paste0(round(x / million, digits = n_decimal), suffix[2]),
                       x_length %in% 10:12 ~ paste0(round(x / billion, digits = n_decimal), suffix[3]),
                       x_length %in% 13:15 ~ paste0(round(x / trillion, digits = n_decimal), suffix[4]),
                       x_length %in% 16:18 ~ paste0(round(x / quadrillion, digits = n_decimal), suffix[5]),
                       x_length %in% 19:21 ~ paste0(round(x / quintillion, digits = n_decimal), suffix[6]),
                       x_length %in% 22:24 ~ paste0(round(x / sextillion, digits = n_decimal), suffix[7]),
                       x_length %in% 25:27 ~ paste0(round(x / septillion, digits = n_decimal), suffix[8]),
                       x_length %in% 28:30 ~ paste0(round(x / octillion, digits = n_decimal), suffix[9]),
                       x_length %in% 31:33 ~ paste0(round(x / nonillion, digits = n_decimal), suffix[10]),
                       x_length %in% 34:36 ~ paste0(round(x / decillion, digits = n_decimal), suffix[11]),
                       TRUE ~ paste0(round(x, digits = n_decimal))
                       )
  
  
  if(is.null(prefix)) {

  options(scipen = scipen_val) # set option back to original setting
  
  return(x_val)
  
  } else {
  
  if(!is.character(prefix)) {
    
  cli_h1("Problem with Input")
    
  cli_abort(c("The object you passed to prefix was of class {.cls {class(prefix)}}",
              "i" = "You must supply a {.cls character} vector of length 1 for the prefix argument of {.fn pretty_number} to work."
              ))
  
  }
    
  x_val <-  paste0(prefix, x_val)
  
  options(scipen = scipen_val)
  
  return(x_val)
    
  }
  
}
