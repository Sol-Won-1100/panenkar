
#' Probit Predict Matches Update
#' 
#' Predict multiple matches using probit, updating the training set in the process
#' 
#' @param fixtures match fixtures, columns of which must be "home_team" and "away_team". For the rest of the parameters
#'        and return see probit_predict_match.R

probit_predict_matches_update <- function (training_set, test_set, market = "result") {
  
  match_dates <- unique(test_set$match_date)
  model_data <- probit_build_model_data(training_set, result, match_dates[1])
  fit <- probit_fit(model_data)
  store_predictions <- list()
  
  for (j in seq_along(1:length(match_dates))) {
    
    fixtures <- filter(test_set, match_date == match_dates[j])
    store_predictions[[j]] <- probit_predict_matches_simple(fixtures, fit)
    
    training_set <- bind_rows(training_set, fixtures)
    
    if (j != length(match_dates)) {
      
      model_data <- probit_build_model_data(training_set, result, match_dates[j + 1])
      fit <- probit_fit(model_data) 
      
    }
    
  }
  
  test_set_predicted <- bind_cols(test_set, bind_rows(store_predictions))
  
  return(test_set_predicted)
  
}

