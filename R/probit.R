
#' Probit Predict Matches Update
#' 
#' Predict multiple matches using probit, updating the training set in the process
#' 
#' @param fixtures match fixtures, columns of which must be "home_team" and "away_team". For the rest of the parameters
#'        and return see probit_predict_match.R

probit_simulate_matches <- function (training_set, test_set, market = "result") {
  
  match_dates <- unique(test_set$match_date)
  model_data <- probit_build_model_data(training_set, result, match_dates[1])
  fit <- probit_fit(model_data)
  store_predictions <- list()
  
  for (j in seq_along(1:length(match_dates))) {
    
    fixtures <- filter(test_set, match_date == match_dates[j])
    store_predictions[[j]] <- probit_predict_matches(fixtures, fit)
    
    training_set <- bind_rows(training_set, fixtures)
    
    if (j != length(match_dates)) {
      
      model_data <- probit_build_model_data(training_set, result, match_dates[j + 1])
      fit <- probit_fit(model_data) 
      
    }
    
  }
  
  test_set_predicted <- bind_cols(test_set, bind_rows(store_predictions))
  
  return(test_set_predicted)
  
}


#' Probit Predict Matches Simple
#' 
#' Predict multiple matches using probit model with no updating of training sets or anything
#' 
#' @param fixtures match fixtures, columns of which must be "home_team" and "away_team". For the rest of the parameters
#'        and return see probit_predict_match.R

probit_predict_matches <- function (fixtures, fit, market = "result") {
  
  if (!is.data.frame(fixtures)) {
    
    stop("'fixtures' must be a data.frame or tibble")
    
  }
  
  if (!("home_team" %in% colnames(fixtures))) {
    
    stop("'fixtures' must have column 'home_team'")
    
  }
  
  if (!("away_team" %in% colnames(fixtures))) {
    
    stop("'fixtures' must have column 'away_team'") 
    
  }
  
  list(home_team = fixtures$home_team, away_team = fixtures$away_team) %>%
    pmap_dfr(probit_predict_match, fit,  market)
  
}


#' Probit Predict Match
#' 
#' Predict a match using ordered probit model
#' 
#' @param home_team the name of the home_team
#' @param away_team the name of the away_team
#' @param fit fitted model using ordered probit
#' @param market only "result" applicable currently
#'        
#' @return a tibble of probabilities for each outcome

probit_predict_match <- function(home_team, away_team, fit, market = "result"){
  
  valid_markets <- "result"
  
  home_team <- unlist(home_team)
  away_team <- unlist(away_team)
  
  if (class(fit) != "clm") {
    
    stop("'fit' must be of class 'clm'")
    
  } 
  
  if (!is.character(home_team)) {
    
    stop ("'home_team' must be of class 'character'")
    
  }
  
  if (!is.character(away_team)) {
    
    stop ("'away_team' must be of class 'character'")
    
  }
  
  length_home_team <- length(home_team)
  length_away_team <- length(away_team)
  
  if (length_home_team != 1) {
    
    stop (glue("'home_team' must be length 1 not {length_home_team}."))
    
  }
  
  if (length_away_team != 1) {
    
    stop (glue("'home_team' must be length 1 not {length_home_team}."))
    
  }
  
  
  if (!is.character(away_team)) {
    
    stop ("'away_team' must be of class 'character'.")
    
  }
  
  
  if (home_team == away_team) {
    
    warning("home_team = away_team, is this correct?")
    
  }
  
  if (sum(market %in% valid_markets) < length(market)) {
    
    stop(glue("'market' must be one or more of {glue_collapse(valid_markets, sep = ', ', last = ' or ')}."))
    
  }
  
  teams_attack <- fit$xlevels$attack
  teams_defence <- fit$xlevels$defence
  
  if (!(home_team %in% teams_attack) | !(home_team %in% teams_defence) | !(away_team %in% teams_attack) |
      !(away_team %in% teams_defence)) {
    
    if (market == "result") {
      
      return(tibble(home_prob = NA_real_,  draw_prob = NA_real_, away_prob = NA_real_))
      
    } 
    
  }
  
  # Construct prediction data
  
  probs <- tibble(location = 1, attack = home_team, defence = away_team) %>%
    predict(fit, ., type = "prob") %>% 
    unlist()
  
  if (market == "result") {
    
    return(tibble(home_prob = probs[1], draw_prob = probs[2], away_prob = probs[3]))
    
  } 
  
}


#' Probit Fit
#' 
#' A wrapper for the polr function with the specs required for order probit model
#' 
#' @param model_data output from probit_build_model_data function
#' 
#' @return fitted model


probit_fit <- function(model_data){
  
  # Polr gives a warning message when it calls glm with probabilistic weights
  
  # suppressWarnings(
  #   polr(result ~ location + attack + defence, data = model_data, weights = time_weight)
  # )
  
  
  suppressWarnings(
    
    clm(result ~ location + attack + defence, data = model_data, weights = time_weight)
    
  )
  
  
  
}

#' @title Probit Build Model Data
#' @description Build model data for probit models based on full time result or goal difference
#' @param results results tibble typically from the live database table results-main
#' @param x variable where the results are, usually 'result' or maybe a half time or second half result variable
#' @param current_date base data for calculating time weights
#' @param xi base parameter in time weight description, Default: 0.0016
#' @return model data tibble ready for input into probit_fit
#' @rdname probit_build_model_data
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
    mutate(location = 1, time_weight = poisson_time_weights(match_date, current_date, xi))
  
  model_data_away <- results %>%
    select(match_date, away_team, home_team, away_result) %>%
    rename(attack = away_team, defence = home_team, result = away_result) %>%
    mutate(location = 0, time_weight = model_data_home$time_weight)
  
  model_data <- bind_rows(model_data_home, model_data_away) %>% 
    drop_na() %>%
    mutate(result = factor(result, levels = c("win", "draw", "loss")))

  return(model_data)
  
}

