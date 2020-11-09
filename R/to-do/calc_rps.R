
#' Calculate Rank Probability Score
#' 
#' Calculates the rank probability score of forecasts
#' 
#' @param predicted a matrix, data frame or tibble of probabilities, each row is a match and each column is an outcome
#'  e.g. home, draw, away. Columns must sum to 1.
#' @param observed a vector of actual outcomes corresponding to the columns in predicted e.g if 1 then the outcome in
#' the first column of predicted actually occurred, you could also pass implied probabilities
#' @param average_all default FALSE is TRUE the usual output, a vector of scores for each match, is averaged so only a
#' single value is returned.
#' @return the rps for each match or average rps score. Smaller score is better.
#' 

calc_rps <- function(predicted, observed, average_all = FALSE){
  
  num_outcomes <- ncol(predicted)
  num_predicted <- nrow(predicted)
  
  rps <- numeric(num_predicted)
  
  for (i in 1:num_predicted){
    
    observed_vector <- rep(0, num_outcomes)
    observed_vector[observed[i]] <- 1
    cumulative <- 0
    
    for (j in 1:num_outcomes){
      
      cumulative <- cumulative + (sum(predicted[i, 1:j]) - sum(observed_vector[1:j])) ^ 2
      
    }
    
    rps[i] <- (1 / (num_outcomes - 1)) * cumulative
    
  }
  
  if (average_all == TRUE) {
    
    rps <- mean(rps, na.rm = TRUE)
    
  }
  
  return(rps)
}
