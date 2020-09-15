
#' @title Build model data poisson
#'
#' @description Reformat the results data into an appropriate format for 
#' modelling using the basic, time-weighted Poisson model
#'
#' @param results results database
#' @param x1 home goals, uses tidy eval so can specify home_goals_half1 etc
#' @param x2 away goals, uses tidy eval so can specify away_goals_half1 etc
#' @param current_date what date to calculate the exponential time weight from
#' @param xi the time weight parameter
#' @param promoted_relegated default FALSE, if true selects the promoted and relegated indicator variables for inclusion
#'       
#' @export

poisson_build_model_data <- function(results, x1, x2, current_date, xi = 0.0016, promoted_relegated = FALSE){
  
  x1 <- enquo(x1)
  x2 <- enquo(x2)
  
  if (promoted_relegated == TRUE) {

    model_data_home <- results %>%
      select(match_date, home_team, away_team, !!x1, home_promoted_into, home_relegated_into, away_promoted_into, 
             away_relegated_into) %>%
      rename(attack_promoted_into = home_promoted_into, 
             attack_relegated_into = home_relegated_into,
             defence_promoted_into = away_promoted_into, 
             defence_relegated_into = away_relegated_into)
    
    model_data_away <- results %>% 
      select(match_date, away_team, home_team, !!x2) %>%
      rename(attack = away_team, defence = home_team, goals = !!x2, home_promoted_into, home_relegated_into, 
             away_promoted_into, away_relegated_into) %>%
      rename(attack_promoted_into = away_promoted_into, 
             attack_relegated_into = away_relegated_into,
             defence_promoted_into = home_promoted_into, 
             defence_relegated_into = home_relegated_into)
    
    promoted_indicators_present <- results %>%
      select(home_promoted_into, away_promoted_into) %>%
      unlist() %>%
      sum(na.rm = TRUE) %>%
      magrittr::is_greater_than(0)
    
    # Some leagues e.g. MLS are closed so wont have any promoted teams so can just drop
    
    if (promoted_indicators_present == FALSE) {
      
      model_data_home <- select(model_data_home, -home_promoted_into, - away_promoted_into)
      model_data_away <- select(model_data_away, -home_promoted_into, - away_promoted_into)
      
    }
    
    relegated_indicators_present <- results %>%
      select(home_relegated_into, away_relegated_into) %>%
      unlist() %>%
      sum(na.rm = TRUE) %>%
      magrittr::is_greater_than(0)    
    
    # Top tier leagues dont have relegations obviously so just drop too
    
    if (relegated_indicators_present == FALSE) {
      
      model_data_home <- select(model_data_home, -home_relegated_into, - away_relegated_into)
      model_data_away <- select(model_data_away, -home_relegated_into, - away_relegated_into)
      
    }    
    
  } else {
    
    model_data_home <- select(results, match_date, home_team, away_team, !!x1) 
    model_data_away <- select(results, match_date, away_team, home_team, !!x2)
     
    
  }
  
  
  model_data_home <- model_data_home %>%
    rename(attack = home_team, defence = away_team, goals = !!x1) %>%
    mutate(location = 1, time_weight = calc_time_weights_exp(match_date, current_date, xi))
  
  model_data_away <- model_data_away %>% 
    select(match_date, away_team, home_team, !!x2) %>%
    rename(attack = away_team, defence = home_team, goals = !!x2) %>%
    mutate(location = 0, time_weight = model_data_home$time_weight)
  
  model_data <- bind_rows(model_data_home, model_data_away) %>%
    drop_na()
  
  return(model_data)
}

