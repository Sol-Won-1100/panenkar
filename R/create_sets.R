

#' @title Create Sets
#'
#' @description Creates a list of test and progressive training sets for fitting
#' betting models, splitting the test sets by match_date
#'
#' @param results_competition football results database filtered so only a 
#' single competition_id
#' @param ... variables in results_competition to keep
#' @param num_seasons_initial number of seasons required for initial training
#' set
#' @param num_seasons_cv number of seasons required for a middle validation set
#' @param min_matches_prediction how many matches in a season must each team 
#' have played before the models prediction is included in profit testing
#' 
#' @note 
#' 
#' Some rules around the training set / test set stuff
#' Use 3 seasons worth of data to initiallise. 
#' Season 1 - only training set
#' Season 2 - only training set
#' Season 3 - only training set
#'
# Season 4 -> 6 we use for cross validation, predicting the n matches of the 
# next date for a particular league before refitting the model.
#' 
#' @export
#'
#' @examples
#' results %>%
#'   read_rds(paste0(wd$data, 'database_results.rds')) %>%
#'   filter(competition_id = "sco_champ") %>%
#'   create_sets()

create_sets <- function(results_competition, ..., num_seasons_initial = 3, 
                        num_seasons_cv = 3, min_matches_prediction = 6){
  
  seasons <- unique(results_competition$season_id)
  
  if(length(seasons) > num_seasons_cv){
    seasons_train_initial <- seasons[1:num_seasons_initial]
    seasons_cv <- seasons[(num_seasons_initial + 1):num_seasons_cv]
  } else {
    stop("number of unique seasons less than or equal to num_seasons_cv")
    
  }
  
  results_competition <- results_competition %>%
    select(...) %>%
    mutate(model_set = case_when(
      season_id %in% seasons_train_initial                ~ "train",
      season_id %in% seasons_cv                           ~ "cv",
      home_match_number_season <= min_matches_prediction  ~ "no_predict",
      away_match_number_season <= min_matches_prediction  ~ "no_predict",
      TRUE                                                ~ "test_profit"
    ))
  
  end_row_initial_train <- max(which(results_competition$model_set == "cv"))
  rows_initial_train <- 1:end_row_initial_train
  rows_initial_test <- (end_row_initial_train + 1):nrow(results_competition)
  
  initial_training_set <- slice(results_competition, rows_initial_train) 
  initial_test_set <- slice(results_competition, rows_initial_test)
  
  test_sets <- group_split(initial_test_set, match_date)
  
  training_sets <- list(initial_training_set)
  
  for(i in 2:length(test_sets)){
    training_sets[[i]] <- bind_rows(training_sets[[i - 1]], 
                                    test_sets[[i - 1]])
  }
  
  print(paste0("Num tests: ", length(test_sets)))
  
  for(i in 1:length(test_sets)){
    test_sets[[i]] <- check_teams(test_sets[[i]], training_sets[[i]])
  }
  
  match_dates <- unique(initial_test_set$match_date)
  
  return(list(training = training_sets, test = test_sets))
}


#' @title Check Teams
#'
#' @description Helper function that checks teams in test set exist in a 
#' training set and filters them out if not
#'
#' @param test_set football results database for testing
#' @param training_set football results database for training

check_teams <- function(test_set, training_set){
  
  teams_train <- training_set %>%
    select(home_team, away_team) %>%
    unlist() %>%
    unique()
  
  test_set_filtered <- test_set %>% 
    filter(home_team %in% teams_train, away_team %in% teams_train)
  
  return(test_set_filtered)
  
}
