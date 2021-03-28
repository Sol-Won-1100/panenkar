
#' @title Build Indicator Matrix
#' @description Build an indicator matrix of 1s and 0s based on a vector of outcomes
#' @param x vector of outcomes, must be a factor
#' @return An indicator matrix
#' @details Each column is a level from x. A 1 indicates the event occurred, a 0 it didn't.
#' @rdname build_indicator_matrix
#' @export 


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
  
  indicator[is.na(x),] <- NA
  
  return(indicator)
  
}


