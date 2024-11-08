###_____________________________________________________________________________
# generate_random_ID() creates random sets of 10 upper/lower case letters and 
# 10 numbers from 1000000000:9999999999 to generate a random ID number
###_____________________________________________________________________________

generate_random_ID <- function(n, set_seed = 12345) {
  # choose whether or not to set the seed
  
  if (is.numeric(set_seed)) {
    set.seed(set_seed)
    
  } else if (is.null(set_seed)) {
    message(
      "Set.seed was not called internally, and so your results will not be consistent across applications.  Make [set_seed] any number to set the seed and make reproducible results!"
    )
    
  }
  
  random_strings <- vector(mode = "character", length = n)
  for (i in 1:n) {
    random_strings[i] <- paste0(paste0(sample(c(
      LETTERS, letters, LETTERS
    ), size = 10), collapse = ""),
    "-",
    sample(1000000000:9999999999, size = 1))
  }
  return(random_strings)
}
