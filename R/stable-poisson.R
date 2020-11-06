
#' Poisson Simulate Matches
#' 
#' Predict multiple matches using Poisson regression, updating the training set in the process
#' 
#' @param training_set, columns of which must be "home_team", "away_team", "home_goals", "away_goals", "match_date"
#' @param test_set, same column restrictions as training_set
#' @param xi parameter for the exponential time weighting, by default 0.0016
#' @param max_goals, maximum number of home goals and/or away goals to model
#' @param markets, vector contain one or more of "result", "over_under" or "both_teams_to_score"
#' @param over_under_goals typically n.5 where n in 1, 2, 3 .. etc, default = 2.5. Argument only used if "over_under"
#' listed in markets
#' @param zero_inflated default FALSE, if TRUE the zero inflated poisson model is used
#' @param weight_cut_off, NA by default can set a lower cut off e.g. 0.01 which will drop observations from model data
#' set to potentially improve speed with a potential accuracy trade off
#' 
#' @return probabilities for outcomes of the various markets for each match in the test set
#' @export

poisson_simulate_matches <- function (training_set, test_set, xi = 0.0016, max_goals = 8, 
                                      markets = c("result", "over_under", "both_teams_to_score"), 
                                      over_under_goals = 2.5, zero_inflated = FALSE, weight_cut_off = NA) {

  
  # Much of he error handling is taken care of by function calls so  not much here
  
  if (!is.logical(zero_inflated)) {
    
    stop("'zero_inflated' must be TRUE or FALSE")
    
  }
  
  # Initialize model from test set
  
  match_dates <- unique(test_set$match_date)
  model_data <- poisson_build_model_data(training_set, home_goals, away_goals, match_dates[1], xi, weight_cut_off)
  fit <- poisson_fit(model_data, zero_inflated)
  store_predictions <- list()

  for (j in seq_along(1:length(match_dates))) {
    
    fixtures <- filter(test_set, match_date == match_dates[j])

    store_predictions[[j]] <- poisson_predict_matches(fixtures, fit, max_goals, markets, over_under_goals)

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


#' Poisson Predict Matches
#' 
#' Predict multiple matches using Poisson regression with no updating of training sets or anything
#' 
#' @param fixtures a tibble columns of which must be "home_team", "away_team", "home_goals", "away_goals"
#' @param fit the fitted poisson or zero-inflated poisson model
#' @param max_goals the maximum number of goals to consider in a match
#' @param markets, vector contain one or more of "result", "over_under" or "both_teams_to_score"
#' @param over_under_goals a number usual 0.5, 1.5, 2.5, etc though could be a whole number. Only comes into play if 
#' markets is "over_under"
#' 
#' @return probabilities for outcomes of the various markets for each match in fixtures
#' @export

poisson_predict_matches <- function (fixtures, fit, max_goals = 8, 
                                     markets =  c("result", "over_under", "both_teams_to_score"), 
                                     over_under_goals = 2.5) {
  
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
    pmap_dfr(poisson_predict_match, 
             fit = fit, 
             max_goals = max_goals, 
             markets = markets, 
             over_under_goals = over_under_goals)
}



#' Poisson Predict Match
#' 
#' Predict a match using poisson regression
#' 
#' @param home_team the name of the home_team
#' @param away_team the name of the away_team
#' @param fit fitted glm using poisson regression
#' @param max_goals the maximum number of goals to consider in a match
#' @param markets, vector contain one or more of "result", "over_under" or "both_teams_to_score"
#' @param over_under_goals a number usual 0.5, 1.5, 2.5, etc though could be a whole number. Only comes into play if 
#'        markets is "over_under"
#'        
#' @return a tibble of probabilities for each outcome

poisson_predict_match <- function(home_team, away_team, fit,  max_goals = 8,  
                                  markets =  c("result", "over_under", "both_teams_to_score"), over_under_goals = 2.5){
  
  # Error handling

  if (class(fit)[1] == "zeroinfl") {
    
    zero_inflated <- TRUE
    
  } else {
    
    zero_inflated <- FALSE
    
    if (class(fit)[1] != "glm" | class(fit)[2] != "lm") {
      
      stop("'fit' must be of class c('glm', 'lm') or 'zeroinfl'")
      
    }
    
  }
  
  if (!is.numeric(max_goals)) {
    
    stop("max_goals must be numeric")
    
  }
  
  if (max_goals <= 0) {
    
    stop("'max_goals' must be greater than 0")
    
  }
  
  if (round(max_goals) != max_goals) {
    
    stop("'max_goals' must be a whole number")
    
  }
  
  if (home_team == away_team) {
    
    warning("home_team = away_team, is this correct?")
    
  }
  
  if ("over_under" %in% markets) {
    
    if (!is.numeric(over_under_goals)) {
      
      stop("over_under_goals must be numeric")
      
    }
    
    if (over_under_goals < 0) {
      
      stop("'over_under_goals' must be less than 0")
      
    }
    
  }
  
  if (!is.logical(zero_inflated)) {
    
    stop("zero_inflated must be TRUE or FALSE")
    
  }
  
  if (!is.character(markets)) {
    
    stop("'markets' must be character vector")  
    
  }
  
  bad_markets <- markets[!(markets %in% c("result", "over_under", "both_teams_to_score"))]
  
  
  if (length(bad_markets) > 0) {
    
    stop(paste0("bad markets supplied must be one of result, over_under, both_teams_to_score"))
    
  }
  
  
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
  
  # A team may have been promoted or relegated into the league and this is their first match so their parameters are not
  # yet present in fit so would return an error
  
  if (!(home_team %in% teams_attack) | !(home_team %in% teams_defence) | !(away_team %in% teams_attack) |
      !(away_team %in% teams_defence)) {

    predictions <- tibble(home_goals_predicted = NA_real_, away_goals_predicted  = NA_real_)
    
    if ("result" %in% markets) {
      
      predictions <- bind_cols(predictions, tibble(home_prob = NA_real_, draw_prob = NA_real_, away_prob = NA_real_))
      
    } 
    
    if ("over_under" %in% markets) {
      
      over_under_col_names <- paste0(c("over", "under"), "_", over_under_goals, "_prob")
      predictions_over_under <- tibble(NA_real_, NA_real_) %>% set_colnames(over_under_col_names)
      
      predictions <- bind_cols(predictions, predictions_over_under)
      
    } 
    
    if ("both_teams_to_score" %in% markets) {
      
      predictions <-bind_cols(predictions, tibble(btts_yes_prob = NA_real_, btts_no_prob = NA_real_))
      
    } 
    
    return(predictions)
    
  }
  
  # Construct prediction data, predict requires a tibble or data frame as input
  
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
    
    # This funny operator creates a tabulation of all possible goals combinations which can be used to derive all other
    # probabilities
    
    goals_table <- home_goals_probs %o% away_goals_probs
    
  }
  
  predictions <- tibble(home_goals_predicted, away_goals_predicted)
  
  if ("result" %in% markets) {
    
    home_prob <- sum(goals_table[lower.tri(goals_table)])
    draw_prob <- sum(diag(goals_table))
    away_prob <- sum(goals_table[upper.tri(goals_table)])
    
    predictions <- bind_cols(predictions, tibble(home_prob, draw_prob, away_prob))
    
  } 
  
  if ("over_under" %in% markets) {
    
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
    
    over_under_col_names <- paste0(c("over", "under"), "_", over_under_goals, "_prob")
    
    predictions_over_under <- tibble(over_prob, under_prob) %>% set_colnames(over_under_col_names)
    
    predictions <- bind_cols(predictions, predictions_over_under)
    
    
  }
  
  if ("both_teams_to_score" %in% markets){
    
    btts_no_prob <- sum(goals_table[1,]) + sum(goals_table[, 1]) - goals_table[1, 1]
    btts_yes_prob <- 1 - btts_no_prob
    
    predictions <- bind_cols(predictions, tibble(btts_yes_prob, btts_no_prob))
    
  } 
  
  return(predictions)
  
}


#' Poisson Fit
#' 
#' A wrapper for the glm function with the specs required for poisson regression model and similar for the zero inflated
#' poisson model
#' 
#' @param model_data result data formated by poisson_build_model_data function
#' @param zero_inflated default FALSE, if TRUE will use zero inflated poisson
#' 
#' @return fitted model
#' @export

poisson_fit <- function (model_data, zero_inflated = FALSE){
  
  if (!is.data.frame(model_data)) {
    
    stop("'model_data' must be a tibble or data frame")
    
  }
  
  model_data_cols <- colnames(model_data)
  expected_cols <- c("match_date", "attack", "defence", "goals", "location", "time_weight")
  missing_cols <- model_data_cols[!(model_data_cols %in% expected_cols)]
  
  if (length(missing_cols) > 0) {
    
    stop(paste0(paste(missing_cols, collapse = ", "), " cols must be in model_data"))
    
  } 
  
  if (!is.logical(zero_inflated)) {
    
    stop("'zero_inflated' must be TRUE or FALSE")
    
  }
  
  if (zero_inflated == FALSE) {
    
    glm(goals ~ location + attack + defence, family = poisson(link = log), data = model_data, weight = time_weight)
    
  } else {
    
    # Often gives warnings about sub-standard fits
    
    suppressWarnings(
      zeroinfl(goals ~ location + attack + defence, data = model_data, weight = time_weight)
    ) 
    
  }
  
}


#' Build model data poisson
#'
#' Reformat the results data into an appropriate format for modelling using the basic, time-weighted Poisson model
#'
#' @param results results database
#' @param x1 home goals, uses tidy eval so can specify half time goals if you like
#' @param x2 away goals, uses tidy eval so can specify half time goals if you like
#' @param current_date what date to calculate the exponential time weight from
#' @param xi the time weight parameter
#' @param weight_cut_off if a weight is less than this cut off that row is dropped from the model data for efficiency
#' purposes
#' 
#' @return formatted tibble ready for using poisson_fit
#'       
#' @export

poisson_build_model_data <- function(results, x1, x2, current_date, xi = 0.0016, weight_cut_off = NA){
  
  x1 <- enquo(x1)
  x2 <- enquo(x2)
  
  model_data_home <- results %>%
    select(match_date, home_team, away_team, !!x1) %>%
    set_colnames(c("match_date", "attack", "defence", "goals")) %>%
    mutate(location = 1, time_weight = poisson_time_weights(match_date, current_date, xi))
  
  model_data_away <- results %>%
    select(match_date, away_team, home_team, !!x2) %>%
    set_colnames(c("match_date", "attack", "defence", "goals")) %>%
    mutate(location = 0, time_weight = model_data_home$time_weight)
  
  model_data <- bind_rows(model_data_home, model_data_away) %>% drop_na() 
  
  if (!is.na(weight_cut_off)) {
    
    model_data <- filter(model_data, time_weight >= weight_cut_off)
    
  }
  
  return(model_data)
}


#' Optimise xi Poisson Goal Difference
#' 
#' Optimise xi parameter of time weighting for poisson model using the difference in goal difference as the evaluation
#' metric
#' 
#' @param xi the value of xi to use in the time weight
#' @param results_estimate_xi results dataset to estimate xi off of
#' @param match_rows rows in the results dataset to estimate xi from
#' @param goal_difference_actual home_goals - away_goals, what really happened to compute mean square error of predicted
#' on
#' 
#' @return mean square error in actual and predicted goal difference
#' @export

optim_xi_poisson_gd <- function(xi, results_estimate_xi, match_rows, goal_difference_actual){
  
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
    
    fit <- glm(goals ~ location + attack + defence, family = poisson(link = log), data = model_data, 
               weight = time_weight)
    
    home_team <- unlist(match[, "home_team"])
    away_team <- unlist(match[, "away_team"])
    
    predict_data_home <- tibble(location = 1, attack = home_team, defence = away_team)
    predict_data_away <- tibble(location = 0, attack = away_team, defence = home_team)
    
    home_goals_predicted <- predict(fit, predict_data_home, type = "response")
    away_goals_predicted <- predict(fit, predict_data_away, type = "response")
    
    goal_difference_predicted[i] <- home_goals_predicted - away_goals_predicted
    
  }
  
  goal_difference_mse <- mean((goal_difference_actual - goal_difference_predicted)^2)
  
  return(goal_difference_mse)
}


#' Calculate Time Weights Exponential
#'
#' Uses an exponetial time weighting for modelling, weighting more recent matches more strongly. This is derived from 
#' the time weighting method used by Dixon-Coles in their version of the double Poisson model
#'
#' @param match_dates dates of football matches, date object
#' @param current_date what date to calculate the exponential time weight from
#' @param xi the time weight parameter
#' 
#' @return vector of weights
#'       
#' @export

poisson_time_weights <- function(match_dates, current_date, xi = 0.0016){
  

  if (class(match_dates) != "Date" ) {
    
    stop ("'match_dates' must be of class Date")
    
  }
  
  if (class(current_date) != "Date" ) {
    
    stop ("'current_date' must be of class Date")
    
  }
  
  if (!is.numeric(xi)) {
    
    stop ("'xi' must be numeric")
    
  }
  
  if (xi <= 0) {
    
    warning ("'xi' less than or equal to 0 may cause problems")
    
  }
  
  days_difference <- as.numeric(current_date - match_dates)
  
  weights <- exp(-xi*days_difference)
  weights[days_difference <= 0] <- 0
  
  return(weights)
  
}
