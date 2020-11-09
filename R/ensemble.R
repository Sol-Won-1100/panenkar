
build_ensemble <- function (predictions, ensemble_weights) {
  
  # Need error checks
  
  num_models <- length(predictions)
  num_outcomes <- ncol(predictions[[1]])
  
  ensemble_weights <- ensemble_weights / sum(ensemble_weights)
  
  ensemble <- matrix(0, nrow = num_matches, ncol = num_outcomes)
  
  for (outcome_col in seq_along(1:num_outcomes)) {
    
    for (model_num in seq_along(1:num_models)) {
      
      ensemble[, outcome_col] <- ensemble[, outcome_col] + ensemble_weights[model_num] * 
        predictions[[model_num]][, outcome_col]      
      
      
    }
    
  }
  
  for (i in 1:nrow(ensemble)) {
    
    ensemble[i,] <- ensemble[i,] / sum(ensemble[i,])
    
  }
  
  return(ensemble)
  
}


calc_ensemble_weights <- function(predictions, observed) {
  
  
  # Check predictions is list
  
  # Check list is > length 1 if not return predictions and warn
  
  # Check each list element is tibble, df or matrix. Convert to matrix.
  
  # Check each list element has same number of rows
  
  # Check observed same number of rows as list element 1
  
  
  start_weights <- rep(1 / length(predictions), length(predictions))
  
  lower_bounds <- rep(0, length(start_weights))
  upper_bounds <- lower_bounds + 1
  
  optim_output <- optim(start_weights, fn = optim_ensemble, predictions = predictions,  observed = observed, 
                        lower = lower_bounds, upper = upper_bounds, method="L-BFGS-B")
  
  final_weights <- optim_output$par / sum(optim_output$par)
  
  return(final_weights)
  
}

optim_ensemble <- function(ensemble_weights, predictions, observed) {
  
  # Check length ensemble_weight > 1
  
  # Check length ensemble weights = length predictions
  
  num_models <- length(ensemble_weights)
  
  # Check strucutre of prediction elements identical and correct
  
  num_matches <- nrow(observed)
  num_outcomes <- ncol(observed)
  
  ensemble <- build_ensemble(predictions, ensemble_weights)
  
  # Check structure of observed identical to prediction element 1
  
  rps <- calc_rps(ensemble, observed, average_all = TRUE)
  
  return(rps)
}