
optim_xi_goal_difference <- function(xi, results_estimate_xi, match_rows, 
                                     goal_difference_actual){
  
  goal_difference_predicted <- numeric(length = length(match_rows))
  
  i <- 0 # Initialize
  
  for(mr in match_rows){
    i <- i + 1
    
    match <- slice(results_estimate_xi, mr)
    
    match_date <- match %>% 
      select(match_date) %>% 
      mutate(match_date = as.character(match_date)) %>%
      unlist() %>%
      ymd()
    
    model_data <- results_estimate_xi %>% 
      slice(1:(mr - 1)) %>%
      build_model_data_poisson(home_goals, away_goals, match_date, xi)
    
    fit <- glm(goals ~ location + attack + defence, family = poisson(link = log), 
               data = model_data, weight = time_weight)
    
    home_team <- unlist(match[, "home_team"])
    away_team <- unlist(match[, "away_team"])
    
    predict_data_home <- tibble(location = 1, attack = home_team, 
                                defence = away_team)
    predict_data_away <- tibble(location = 0, attack = away_team, 
                                defence = home_team)
    
    home_goals_predicted <- predict(fit, predict_data_home, type = "response")
    away_goals_predicted <- predict(fit, predict_data_away, type = "response")
    
    goal_difference_predicted[i] <- home_goals_predicted - away_goals_predicted
    
  }
  
  goal_difference_mse <- mean((goal_difference_actual - goal_difference_predicted)^2)
  
  return(goal_difference_mse)
}