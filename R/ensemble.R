

#' Build Ensemble
#' 
#' Combine a list of predictions using given weights
#' 
#' @param predictions a list containing match probabilities, each column is an outcome, each row a match. Each list
#' element must be the same structure
#' @param ensemble_weights a vector of weights
#' 
#' @return a matrix of weighted probabilities
#' @export

build_ensemble <- function (predictions, ensemble_weights) {
  
  if (!is.list(predictions)) {
    
    stop("'predictions' must be a list")
    
  }
  
  num_models <- length(predictions)
  
  if(num_models < 2) {
    
    error_message <- paste0("'predictions' list must be of length 2 or greater not ", num_models, ".")
    
    stop(error_message)
    
  }

  is_prediction_class_ok <- map_lgl(predictions, ~is.data.frame(.) | is.matrix(.))
  
  if (sum(is_prediction_class_ok) != length(is_prediction_class_ok)) {
    
    stop("'predictions' elements must be of class data.frame, tibble or matrix")
    
  }
  
  num_outcomes <- ncol(predictions[[1]])
  num_matches <- nrow(predictions[[1]])
  
  
  for (model_num in seq_along(2:num_models)) {
    
    num_outcomes_next <- ncol(predictions[[model_num]])
    num_matches_next <- nrow(predictions[[model_num]])
    
    if (num_outcomes != num_outcomes_next) {
      
      error_message <- paste0("'prediction' element ", model_num, " must have same number of cols (", num_outcomes,
                              " cols) as 'prediction' element ", model_num, " (", num_outcomes_next)
      
      stop(error_message)
      
    }
    
    if (num_matches != num_matches_next) {
      
      error_message <- paste0("'prediction' element ", model_num, " must have same number of rows (", num_matches,
                              " rows) as 'prediction' element ", model_num, " (", num_matches_next)
      
      stop(error_message)
      
    }
    
  }
  
  if (num_models != length(ensemble_weights)) {
    
    stop("'predictions' must have same length as 'ensemble_weights'.")
    
  }
  
  predictions <- map(predictions, ~matrix(unlist(.) ,nrow = num_matches, ncol = num_outcomes))

  is_element_numeric <- map_lgl(predictions, is.numeric)
  
  if (sum(is_element_numeric) != length(is_element_numeric)) {
    
    stop("'predictions' elements must be numeric or coercable to numeric.")
    
  }
  
  if (!is.numeric(ensemble_weights)) {
    
    stop("'ensemble_weights' must be numeric")
    
  }

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


#' Calculate Ensemble Weights
#' 
#' Optimises a set of weights given a list of separate match outcome probabilities and the observed probabilities
#' 
#' @param predictions see \code{\link{build_ensemble}}
#' @param observed actual outcomes, possibly closing implied probs from closing odds or 0 1 indicators for actual 
#' outcomes
#' 
#' @return a vector of optimised weights
#' @export

calc_ensemble_weights <- function(predictions, observed) {
  
  if (!is.list(predictions)) {
    
    stop("'predictions' must be a list")
    
  }
  
  num_models <- length(predictions)
  
  if(num_models < 2) {
    
    error_message <- paste0("'predictions' list must be of length 2 or greater not ", num_models, ".")
    
    stop(error_message)
    
  }
  
  is_prediction_class_ok <- map_lgl(predictions, ~is.data.frame(.) | is.matrix(.))
  
  if (sum(is_prediction_class_ok) != length(is_prediction_class_ok)) {
    
    stop("'predictions' elements must be of class data.frame, tibble or matrix")
    
  }
  
  num_outcomes <- ncol(predictions[[1]])
  num_matches <- nrow(predictions[[1]])
  
  for (model_num in seq_along(2:num_models)) {
    
    num_outcomes_next <- ncol(predictions[[model_num]])
    num_matches_next <- nrow(predictions[[model_num]])
    
    if (num_outcomes != num_outcomes_next) {
      
      error_message <- paste0("'prediction' element ", model_num, " must have same number of cols (", num_outcomes,
                              " cols) as 'prediction' element ", model_num, " (", num_outcomes_next)
      
      stop(error_message)
      
    }
    
    if (num_matches != num_matches_next) {
      
      error_message <- paste0("'prediction' element ", model_num, " must have same number of rows (", num_matches,
                              " rows) as 'prediction' element ", model_num, " (", num_matches_next, ".")
      
      stop(error_message)
      
    }
    
  }
  
  if (num_matches != nrow(observed)) {
    
    stop("'predicted' elements and observed must have same number of rows.")
    
  }
  
  if (num_outcomes != ncol(observed)) {
    
    stop("'predicted' elements and observed must have same number of cols.")
    
  }
  
  
  predictions <- map(predictions, ~matrix(unlist(.) ,nrow = num_matches, ncol = num_outcomes))
  is_element_numeric <- map_lgl(predictions, is.numeric)
  
  if (sum(is_element_numeric) != length(is_element_numeric)) {
    
    stop("'predictions' elements must be numeric or coercable to numeric.")
    
  }
  
  start_weights <- rep(1 / length(predictions), length(predictions))
  
  lower_bounds <- rep(0, length(start_weights))
  upper_bounds <- lower_bounds + 1
  
  optim_output <- optim(start_weights, fn = optim_ensemble, predictions = predictions,  observed = observed, 
                        lower = lower_bounds, upper = upper_bounds, method="L-BFGS-B")
  
  final_weights <- optim_output$par / sum(optim_output$par)
  
  return(final_weights)
  
}

#' Optimize Ensemble
#' 
#' Helper function for deriviing optimum model weights
#' 
#' @param ensemble_weights see \code{\link{build_ensemble}}
#' @param predictions see \code{\link{build_ensemble}}
#' @param observed see \code{\link{calc_ensemble_Weights}}
#' 
#' @return rps for that weighting

optim_ensemble <- function(ensemble_weights, predictions, observed) {
  
  # This functions is only called from calc_ensemble_weight which has extensive error handling on predictions and 
  # observed so I am choosing to largely omit here.
  
  num_models <- length(ensemble_weights)
  
  # Check strucutre of prediction elements identical and correct
  
  num_matches <- nrow(observed)
  num_outcomes <- ncol(observed)
  
  ensemble <- build_ensemble(predictions, ensemble_weights)
  
  # Check structure of observed identical to prediction element 1
  
  rps <- calc_rps(ensemble, observed)
  
  return(rps)
}