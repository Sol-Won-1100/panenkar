


build_indicator_matrix <- function(x) {
  
  if (!is.factor(x)) {
    
    stop("'x' must be a factor")
    
  }
  
  values <- levels(x)
  
  indicator <- matrix(0, nrow = length(x), ncol= length(values))
  
  colnames(indicator) <- values
  
  
  for (i in seq_along(1:ncol(indicator))) {
    
    indicator[x == values[i], values[i]] <- 1
    
  }
  
  return(indicator)
  
}
