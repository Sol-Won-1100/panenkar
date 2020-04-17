
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
#'       
#' @export

build_model_data_poisson <- function(results, x1, x2, current_date, 
                                     xi = 0.0016){
  
  x1 <- enquo(x1)
  x2 <- enquo(x2)
  
  model_data_home <- results %>% 
    select(match_date, home_team, away_team, !!x1) %>%
    rename(attack = home_team, defence = away_team, goals = !!x1) %>%
    mutate(location = 1,
           time_weight = calc_time_weights_exp(match_date, current_date, xi))
  
  model_data_away <- results %>% 
    select(match_date, away_team, home_team, !!x2) %>%
    rename(attack = away_team, defence = home_team, goals = !!x2) %>%
    mutate(location = 0,
           time_weight = model_data_home$time_weight)
  
  model_data <- bind_rows(model_data_home, model_data_away) %>%
    drop_na()
  
  return(model_data)
}

