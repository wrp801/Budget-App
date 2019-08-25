cost_lookup <- function(category) {
  
  index <- which(recent_avg[,1] == category)
  
  if(length(index) == 0) {
    
    return(0)
    
  } else {
    
    val <- recent_avg[index,2]
    
    return(round(pull(val),digits = 0))
    
  }
  
}
