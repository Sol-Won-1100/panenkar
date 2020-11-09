
#' Calculate Rank Probability Score
#' 
#' Calculates the rank probability score of forecasts
#' 
#' @param predicted a matrix, data frame or tibble of probabilities, each row is a match and each column is an outcome
#'  e.g. home, draw, away. Columns must sum to 1.
#' @param observed a vector of actual outcomes corresponding to the columns in predicted e.g if 1 then the outcome in
#' the first column of predicted actually occurred, you could also pass implied probabilities
#' @param check_predicted_row_sums if TRUE this will check that row sums in predicted are 1 at a tolerance level of 
#' 0.0001.
#' 
#' @return the rps for each match or average rps score. Smaller score is better.
#' @export

calc_rps <- function(predicted, observed, check_predicted_row_sums = TRUE){
  
  if (!is.data.frame(predicted) & !is.matrix(predicted)) {
    
    stop("'predicted' must be a data.frame, tibble or matrix.")
    
  }
  
  if (!is.data.frame(observed) & !is.matrix(observed)) {
    
    stop("'observed' must be a data.frame, tibble or matrix.")
    
  }
  
  if(!is.logical(check_predicted_row_sums)) {
    
    stop("'check_predicted_row_sums' must be TRUE or FALSE.")
    
  }

  
  num_matches <- nrow(predicted)
  num_outcomes <- ncol(predicted)
  
  num_matches_observed <- nrow(observed)
  num_outcomes_observed <- ncol(observed)
  
  
  if (num_matches != num_matches_observed) {
    
    error_message <- paste(num_matches, "rows (matches) in predicted and", num_matches_observed, 
                           "in observed. Expected same number of rows.")
    
    stop(error_message)
    
  }
  
  if (num_outcomes != num_outcomes_observed) {
    
    error_message <- paste(num_observed, "columns (outcomes) in predicted and", num_outcomes_observed, 
                           "in observed. Expected same number of columns.")
    
    stop(error_message)
    
  }
  
  if (num_outcomes < 2) {
    
    stop ("There must be 2 or more columns (outcomes) in both 'predicted' and 'observed'.")
    
  }
  
  if (num_matches == 0) {
    
    stop ("There must be 1 or more rows (matches) in both 'predicted' and 'observed'.")
    
  }
  
  predicted <- as.matrix(predicted, nrow = num_matches, ncol = num_outcomes)
  observed <- as.matrix(observed, nrow = num_matches, ncol = num_outcomes)
  
  if(!is.numeric(predicted)) {
    
    stop("'predicted' must be numeric.")
    
  }
  
  if(!is.numeric(observed)) {
    
    stop("'observed' must be numeric.")
    
  }  
  
  if (check_predicted_row_sums == TRUE) {
    
    predicted_row_sums <- predicted %>% rowSums() %>% round(3) # A tolerance of 0.001
    bad_predcited_row_sums <- predicted_row_sums[predicted_row_sums != 1]
    
    if (length(bad_predcited_row_sums) > 0) {
      
      stop ("1 or more 'predicted' rows do not sum to 1 at tolerance level.")
      
    }
    
  }

  rps <- numeric(num_matches)
  
  for (i in seq_along(1:num_matches)){
    
    observed_vector <- rep(0, num_outcomes)
    observed_vector[observed[i]] <- 1
    cumulative <- 0
    
    for (j in 1:num_outcomes){
      
      cumulative <- cumulative + (sum(predicted[i, 1:j]) - sum(observed_vector[1:j])) ^ 2
      
    }
    
    rps[i] <- (1 / (num_outcomes - 1)) * cumulative
    
  }

  rps <- mean(rps, na.rm = TRUE)

  return(rps)
  
}
