
#' Poisson Predict Matches Update
#' 
#' Predict multiple matches using Poisson regression, updating the training set in the process
#' 
#' @param fixtures match fixtures, columns of which must be "home_team" and "away_team". For the rest of the parameters
#'        and return see poisson_predict_match.R

poisson_predict_matches_update <- function (training_set, test_set, max_goals = 8, market = "result", 
                                            over_under_goals = 2.5, promoted_relegated = FALSE) {
  
  match_dates <- unique(test_set$match_date)
  model_data <- poisson_build_model_data(training_set, home_goals, away_goals, match_dates[1], promoted_relegated)
  fit <- poisson_fit(model_data)
  store_predictions <- list()
  
  for (j in seq_along(1:length(match_dates))) {
    
    fixtures <- filter(test_set, match_date == match_dates[j])
    store_predictions[[j]] <- poisson_predict_matches_simple(fixtures, fit)
    
    training_set <- bind_rows(training_set, fixtures)
    
    if (j != length(match_dates)) {
      
      model_data <- poisson_build_model_data(training_set, home_goals, away_goals, match_dates[j + 1], 
                                             promoted_relegated)
      fit <- poisson_fit(model_data) 
      
    }

  }
  
  test_set_predicted <- bind_cols(test_set, bind_rows(store_predictions))
  
  return(test_set_predicted)
  
}

