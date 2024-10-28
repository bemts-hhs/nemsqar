################################################################################
### orRify function ############################################################
################################################################################

orRify <- function(string){
  
  out <- character()
  
  for (i in 1:(length(string) - 1)) {
    
    a <- string[i]
    
    out <- paste(out, a, "|", sep = "")
    
  }
  
  out <- paste(out, string[length(string)], sep = "")
  
  out
  
}
