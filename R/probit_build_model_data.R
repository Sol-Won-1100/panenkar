
#' @title Build model data probit
#'
#' @description Reformat the results data into an appropriate format for modelling using probit model
#'
#' @param results results database
#' @param x result variable
#' @param current_date what date to calculate the exponential time weight from
#' @param xi the time weight parameter
#'       
#' @export

probit_build_model_data <- function(results, x, current_date, xi = 0.0016){
  
  x <- enquo(x)

  results <- mutate(results,
                    home_result = case_when(!!x == "home" ~ "win",
                                            !!x == "draw" ~ "draw",
                                            !!x == "away" ~ "loss",
                                            TRUE ~ NA_character_),
                    away_result = case_when(!!x == "home" ~ "loss",
                                            !!x == "draw" ~ "draw",
                                            !!x == "away" ~ "win",
                                            TRUE ~ NA_character_))

  model_data_home <- results %>%
    select(match_date, home_team, away_team, home_result) %>%
    rename(attack = home_team, defence = away_team, result = home_result) %>%
    mutate(location = 1, time_weight = calc_time_weights_exp(match_date, current_date, xi))

  model_data_away <- results %>%
    select(match_date, away_team, home_team, away_result) %>%
    rename(attack = away_team, defence = home_team, result = away_result) %>%
    mutate(location = 0, time_weight = model_data_home$time_weight)

  model_data <- bind_rows(model_data_home, model_data_away) %>%
    drop_na() %>%
    mutate(result = factor(result, levels = c("win", "draw", "loss")))
  
  # model_data <- results %>%
  #   rename(result = !!x) %>%
  #   select(match_date, home_team, away_team, result) %>%
  #   drop_na() %>%
  #   mutate(time_weight = calc_time_weights_exp(match_date, current_date, xi),
  #          result = factor(result, levels = c("home", "draw", "away")))
  
  return(model_data)
}

