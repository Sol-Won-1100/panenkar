

# Just about there need to finish the simulation function. There was a warning getting flashed up from the gd_calc_rating
# function which is odd so investigate that. Once done the simulation function you will want to run the optim function
# to derive xi and test a few values of gd.


# Exports --------------------------------------------------------------------------------------------------------------

#' @title Goal Difference Model Simulate Matches
#' @description Simulate matches using the basic goal difference rating model
#' @param training_set, columns of which must be "home_team", "away_team", ("result" and/or "over_under_2_5" or similar
#' depending on the market specified) and "match_date"
#' @param test_set, same column restrictions as training_set, the set to get outcome probabilities for
#' @param xi the time weight parameter for weighting the historic goal difference to derive the rating. This is just 
#' using the same Poisson regression time weighting approach but xi may be different
#' @param max_gd maximum goal difference to consider, if > than this value it is trimmed, Default: NA results in no
#' trimming. Trimming may be helpful for removing outliers.
#' @param min_matches minimum number of matches a team must have played before a rating is assigned. Default: 10.
#' @param markets which markets to calculate probabilities for, Default: c("result", "over_under")
#' @param over_under_goals used if '"over_under" present in markets, Default: 2.5
#' @param bind_to_test if FALSE just the probabilties will be outputted. THis is good for optimising parameters, 
#' Default: TRUE
#' @return the predicted probabilities possibly binded to the test set
#' @rdname gd_simulate_matches
#' @export 

gd_simulate_matches <- function (training_set, test_set, xi, max_gd = NA, min_matches = 10, 
                                 markets = c("result", "over_under"), over_under_goals = 2.5) {
  
  # Add match ratings to the training set, these will be used for fitting the model on. As we loop through the test set
  # this will grow.
  
  training_set <- gd_add_ratings(training_set, xi, max_gd, min_matches)
  
  # Factors are needed for fitting models
  
  if ("result" %in% markets) {
    
    training_set <- mutate(training_set, result = factor(result, c("home", "draw", "away")))
    test_set <- mutate(test_set, result = factor(result, c("home", "draw", "away")))
    
  }
  
  if ("over_under" %in% markets) {
    
    over_under_var <- paste0("over_under_", str_replace(over_under_goals, "\\.", "_"))
    
    training_set[, over_under_var] <- training_set[, over_under_var] %>% 
      unlist() %>% 
      factor(levels = c("over", "under"))
    
    test_set[, over_under_var] <- test_set[, over_under_var] %>% 
      unlist() %>% 
      factor(levels = c("over", "under"))    
    
  }
  
  # Initialize
  
  test_set <- mutate(test_set, home_rating = NA_real_, away_rating = NA_real_, match_rating = NA_real_)

  match_dates <- unique(test_set$match_date)
  fit <- clm(result ~ match_rating, data = training_set)
  store_predictions <- list()
  
  # Predict matches and update training set and model

  for (i in seq_along(1:length(match_dates))) {
    
    match_date_i <- match_dates[i]
    
    fixtures <- test_set %>% 
      filter(match_date == match_dates[i]) %>% 
      mutate(home_rating = gd_calc_ratings(training_set, home_team, match_date_i, xi, max_gd, min_matches),
             away_rating = gd_calc_ratings(training_set, away_team, match_date_i, xi, max_gd, min_matches),
             match_rating = home_rating - away_rating)
    
    
    store_predictions[[i]] <- gd_predict_matches(training_set, fit, fixtures$match_rating, market = "result")
    
    training_set <- bind_rows(training_set, fixtures)
    
    if (i != length(match_dates)) {
      
      fit <- clm(result ~ match_rating, data = training_set)
      
    }
    
  }

  test_set_predicted <- bind_cols(test_set, bind_rows(store_predictions))
  
  return(test_set_predicted)
  
}


#' @title Goal Difference Model Predict Matches
#' @description Predict matches using the basic goal difference rating model
#' @param historic_results results to be used for the fitting the model
#' @param fit fitted probit model using clm, either results ~ match_rating or over_under_2_5 (etc) ~ match_rating
#' @param match_rating defined as the home_rating - away_rating, where a teams rating is exp weighted average of 
#' previous matches goal difference see \code{\link{gd_calc_ratings}}
#' @param market see \code{\link{gd_simulate_matches}}
#' @param over_under_goals see \code{\link{gd_simulate_matches}}
#' @return match probabilities
#' @rdname gd_predict_matches
#' @export 

gd_predict_matches <- function(historic_results, fit, match_rating, market = "result", over_under_goals = 2.5) {
  
  probs <- tibble(match_rating = match_rating) %>%
    predict(fit, ., type = "prob")
  
  probs <- probs$fit
  
  if (market == "result") {
    
    colnames(probs) <- probs %>% colnames() %>% paste0("_prob")
    
  } else {
    
    over_under_cols <- paste0(c("over", "under"), str_replace(over_under_goals, "\\.", "_"))
    colnames(probs) <- over_under_cols
  }
  
  return(as_tibble(probs))
  
}


#' @title Goal Difference Model Calculate Rating
#' @description Predict matches using the basic goal difference rating model
#' @param historic_results results to be used for the fitting the model
#' @param fit fitted probit model using clm, either results ~ match_rating or over_under_2_5 (etc) ~ match_rating
#' @param match_rating defined as the home_rating - away_rating, where a teams rating is exp weighted average of 
#' previous matches goal difference
#' @param market see \code{\link{gd_simulate_matches}}
#' @param over_under_goals see \code{\link{gd_simulate_matches}}
#' @return match probabilities
#' @rdname gd_predict_matches
#' @export 

gd_calc_ratings <- function(results, teams, current_date, xi, max_gd, min_matches) {
  
  list(.team = teams) %>%
    pmap_dbl(gd_calc_rating, results = results, current_date = current_date, xi = xi, max_gd = max_gd, 
             min_matches = min_matches)
  
}



#' @title Goal Difference Model Optimize Xi
#' @description Optimize the xi time weight parameter for the goal difference model
#' @param training_set see \code{\link{gd_simulate_matches}}
#' @param test_set see \code{\link{gd_simulate_matches}}
#' @param start_xi starting value of xi to try
#' @param max_gd see \code{\link{gd_simulate_matches}}
#' @param min_matches see \code{\link{gd_simulate_matches}}
#' @return list, optim output including value and optimal parameters
#' @rdname gd_optim_xi
#' @export 

gd_optim_xi <- function(training_set, test_set, start_xi, max_gd, min_matches) {
  
  optim(start_xi, fn = .gd_optim_xi, training_set = training_set, test_set = test_set, max_gd = max_gd, 
        min_matches = min_matches)
  
} 


#' @title Goal Difference Model Add Ratings
#' @description Add match rating to results tibble
#' @param results results data set
#' @param xi time weight parameters
#' @param max_gd maximum goal difference, Default: NA
#' @param min_matches minimum matches, Default: 10
#' @return results with home_rating, away_rating and match_rating columns
#' @rdname gd_add_ratings
#' @export 

gd_add_ratings <- function(results, xi, max_gd = NA, min_matches = 10) {
  
  results <- mutate(results, home_rating = NA_real_, away_rating = NA_real_)
  
  for (i in (min_matches + 1):nrow(results)) {
    
    historic_results <- results[1:(i - 1), ]
    home_team <- unlist(results[i, "home_team"])
    away_team <- unlist(results[i, "away_team"])
    match_date <- results %>% 
      slice(i) %>% 
      select(match_date) %>% 
      mutate(match_date = as.character(match_date)) %>%
      unlist() %>%
      as.Date()
    
    results[i, "home_rating"] <- gd_calc_rating(historic_results, home_team, match_date,  xi, max_gd)
    results[i, "away_rating"] <- gd_calc_rating(historic_results, away_team, match_date,  xi, max_gd)
    
  }
  
  results <- mutate(results, match_rating = home_rating - away_rating)
  
  return(results)
  
}




# Helpers --------------------------------------------------------------------------------------------------------------

# Calculate a rating for a single team given a set of historic results. THis is the helper function for gd_calc_ratings
# where you can supply multiple teams. The arguments are detailed in other functions so I wont document.

gd_calc_rating <- function (results, .team, current_date, xi, max_gd = 3, min_matches = 10) {
  
  gd_check_error_results(results)
  
  if (length(.team) != 1) {
    
    stop (glue("'.team' must be of length 1, not {length(team)}."))
    
  }
  
  if (!is.character(.team)) {
    
    stop ("'.team' must be of class character.")
    
  }
  
  gd_check_error_current_date(current_date)
  gd_check_error_xi(xi)
  gd_check_error_max_gd(max_gd)
  gd_check_error_min_matches(min_matches)
  

  

  
  
  team_results <- results %>% 
    mutate(gd = home_goals - away_goals) %>%
    select(match_date, home_team, away_team, gd) %>%
    pivot_longer(cols = c("home_team", "away_team"), names_to = "location", values_to = "team") %>%
    filter(team == .team) 
  
  
  if (nrow(team_results) < min_matches) {
    
    return(NA_real_)
    
  }
  
  team_results <- team_results %>%
    arrange(match_date) %>%
    mutate(time_weight = poisson_time_weights(match_date, current_date, xi),
           time_weight = time_weight / sum(time_weight, na.rm = TRUE),
           gd = if_else(location == "away_team", -gd, gd),
           gd = as.numeric(gd))
  
  
  if (!is.na(max_gd)) {
    
    team_results <- mutate(team_results, gd = case_when(gd > max_gd ~ max_gd,
                                                        gd < -max_gd ~ -max_gd,
                                                        TRUE ~ gd))
    
  }
  
  team_results <- mutate(team_results, rating = time_weight * gd)
  
  rating <- sum(team_results$rating, na.rm = TRUE)
  
  return(rating)
  
}



gd_check_error_results <- function (results) {
  
  if (!is.data.frame(results)) {
    
    stop("'results' must be a tibble or data.frame")
    
  }
  
  expected_cols <- c("home_team", "away_team", "home_goals", "away_goals", "match_date")
  
  if (sum(expected_cols %in% colnames(results)) != length(expected_cols)) {
    
    stop (glue("'results' must have cols {glue_collapse(expected_cols, sep = ', ', last = ' or ')}."))
    
  }
  
  if (class(results$match_date) != "Date") {
    
    stop("'results' column 'match_date' must be of class Date.")
    
  }
  
  return(results)
  
}

gd_check_error_current_date <- function (current_date) {
  
  if (length(current_date) != 1) {
    
    stop (glue("'current_date' must be of length 1, not {length(current_date)}."))
    
  }
  
  if (!is.Date(current_date)) {
    
    stop ("'current_date' must be of class Date.")
    
  }
  
}

gd_check_error_xi <- function (xi) {
  
  if (length(xi) != 1) {
    
    stop (glue("'xi' must be of length 1, not {length(xi)}."))
    
  }
  
  if (!is.numeric(xi)) {
    
    stop ("'current_date' must be of class Date.")
    
  }  
  
  return(xi)
  
}


gd_check_error_max_gd <- function (max_gd) {
  
  if (length(max_gd) != 1) {
    
    stop (glue("'max_gd' must be of length 1, not {length(max_gd)}."))
    
  }
  
  if (!is.numeric(max_gd)) {
    
    stop ("'max_gd' must be of class numeric.")
    
  }  
  
  if (max_gd < 1) {
    
    stop ("'max_gd' must be greater than or equal to 1.")
    
  }
  
  if (round_number(max_gd) != max_gd) {
    
    stop ("'max_gd' must be whole number.")
    
  }  

  return(max_gd)
  
}

# This function is the real work horse of the gd_optim_xi function. This is the input to Rs built in optimiser with the
# aim of obtain the best value of xi for the job. All the arguments are detailed in other functions so I am not 
# documenting as this wont be exported anyway.

.gd_optim_xi <- function (xi, training_set, test_set, max_gd = NA, min_matches = 10) {
  
  predicted <- gd_simulate_matches(training_set, test_set, xi, max_gd , min_matches = 10, markets = "result")
  
  rps <- predicted %>% select(home_prob, draw_prob, away_prob) %>% calc_rps(observed)
  
  return(rps)
}
