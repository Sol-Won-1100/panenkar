
#' Poisson Predict Matches Update
#' 
#' Predict multiple matches using Poisson regression, updating the training set in the process
#' 
#' @param fixtures match fixtures, columns of which must be "home_team" and "away_team". For the rest of the parameters
#'        and return see poisson_predict_match.R

poisson_predict_matches_update <- function (training_set, test_set, xi = 0.0016, max_goals = 8, market = "result", 
                                            over_under_goals = 2.5, zero_inflated = FALSE, weight_cut_off = NA,
                                            mark_promoted_relegated = TRUE) {

  # Come back to this once you have added in the features - see trello
  if (mark_promoted_relegated == TRUE) {
    
    train_test_combi <- bind_rows(training_set, test_set)
    
    home_promoted <- train_test_combi %>%
      select(home_team, home_promoted_into, season_id) %>%
      distinct() %>%
      filter(home_promoted_into == 1) %>%
      mutate(team = home_team, season_id = previous_season(season_id)) %>%
      select(team, season_id)
    
    home_promoted <- train_test_combi %>%
      select(home_team, home_promoted_into, season_id) %>%
      distinct() %>%
      filter(home_promoted_into == 1) %>%
      rename(team = home_team) %>%
      select(team, season_id)    
    
    
    
  }

  
  
  match_dates <- unique(test_set$match_date)
  model_data <- poisson_build_model_data(training_set, home_goals, away_goals, match_dates[1], xi, weight_cut_off)
  fit <- poisson_fit(model_data, zero_inflated)
  store_predictions <- list()

  for (j in seq_along(1:length(match_dates))) {
    
    fixtures <- filter(test_set, match_date == match_dates[j])

    store_predictions[[j]] <- poisson_predict_matches_simple(fixtures, fit, max_goals, market, over_under_goals, 
                                                             zero_inflated)

    training_set <- bind_rows(training_set, fixtures)
    
    if (j != length(match_dates)) {
      
      model_data <- poisson_build_model_data(training_set, home_goals, away_goals, match_dates[j + 1], xi, 
                                             weight_cut_off)
      
      fit <- poisson_fit(model_data, zero_inflated) 
      
    }

  }
  
  test_set_predicted <- bind_cols(test_set, bind_rows(store_predictions))
  
  return(test_set_predicted)
  
}

