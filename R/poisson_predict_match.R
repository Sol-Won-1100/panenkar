
#' Poisson Predict Match
#' 
#' Predict a match using poisson regression
#' 
#' @param home_team the name of the home_team
#' @param away_team the name of the away_team
#' @param fit fitted glm using poisson regression
#' @param max_goals the maximum number of goals to consider in a match
#' @param market one of "result", "over_under" or "both_teams_to_score"
#' @param over_under_goals a number usual 0.5, 1.5, 2.5, etc though could be a whole number. Only comes into play if 
#'        market is "over_under"
#' @param zero_inflated default FALSE, if TRUE will use zero inflated poisson
#'        
#' @return a tibble of probabilities for each outcome


poisson_predict_match <- function(home_team, away_team, fit,  max_goals = 8, market = "result", over_under_goals = 2.5,
                                  zero_inflated = FALSE){
  
  # Consistency checks
  
  home_team <- unlist(home_team)
  away_team <- unlist(away_team)
  
  if (zero_inflated == FALSE) {
    
    teams_attack <- fit$xlevels$attack
    teams_defence <- fit$xlevels$defence    
    
  } else {
    
    teams_attack <- fit$levels$attack
    teams_defence <- fit$levels$defence        
    
    
  }


  if (!(home_team %in% teams_attack) | !(home_team %in% teams_defence) | !(away_team %in% teams_attack) |
      !(away_team %in% teams_defence)) {
    
    if (market == "result") {
      
      return(tibble(home_goals_predicted = NA_real_, away_goals_predicted  = NA_real_, home_prob = NA_real_, 
                    draw_prob = NA_real_, away_prob = NA_real_))
      
    } else if (market == "over_under") {
      
      return(tibble(home_goals_predicted = NA_real_, away_goals_predicted  = NA_real_, over_prob = NA_real_, 
                    under_prob = NA_real_))
      
    } else if (market == "both_teams_to_score") {
      
      return(tibble(home_goals_predicted = NA_real_, away_goals_predicted  = NA_real_, yes_prob = NA_real_, 
                    no_prob = NA_real_))
      
    } else {
      
      stop("bad market supplied")
      
    }
    
  }
  
  # Construct prediction data
  
  predict_data_home <- tibble(location = 1, attack = home_team, defence = away_team)
  predict_data_away <- tibble(location = 0, attack = away_team, defence = home_team)
  
  home_goals_predicted <- predict(fit, predict_data_home, type = "response")
  away_goals_predicted <- predict(fit, predict_data_away, type = "response")
  
  # Tabulate outcomes
  
  if (zero_inflated == FALSE) {
    
    goals_table <- dpois(0:max_goals, home_goals_predicted) %o% dpois(0:max_goals, away_goals_predicted)
    
  } else {
    
    home_goals_probs <- predict(fit, predict_data_home, type = "prob") %>%
      t() %>%
      as_tibble() %>%
      unlist()
      
      
    away_goals_probs <- predict(fit, predict_data_away, type = "prob") %>%
      t() %>%
      as_tibble() %>%
      unlist()
    
    goals_table <- home_goals_probs %o% away_goals_probs

  }
  
  if (market == "result") {

    home_prob <- sum(goals_table[lower.tri(goals_table)])
    draw_prob <- sum(diag(goals_table))
    away_prob <- sum(goals_table[upper.tri(goals_table)])
    
    return(tibble(home_goals_predicted, away_goals_predicted, home_prob, draw_prob, away_prob))
    
  } else if (market == "over_under") {
    
    over_prob <- 0
    under_prob <- 0
    
    for (i in 1:nrow(goals_table)) {
      
      for (j in 1:ncol(goals_table)) {
        
        if ((i - 1) + (j - 1) > over_under_goals){
          
          over_prob <- over_prob + goals_table[i, j]
          
        } else {
          
          under_prob <- under_prob + goals_table[i, j]
          
        }
        
      }
      
    }
    
    return(tibble(home_goals_predicted, away_goals_predicted, over_prob, under_prob))
    
  } else if (market == "both_teams_to_score"){
    
    no_prob <- sum(goals_table[1,]) + sum(goals_table[, 1]) - goals_table[1, 1]
    yes_prob <- 1 - no_prob
    
    return(tibble(home_goals_predicted, away_goals_predicted, yes_prob, no_prob))
    
  } else {
    
    stop("bad market supplied")
    
  }
  
}
