
#' @title Add Independent Features Football Data
#'
#' @description Add time independent features to football data results dataset. 
#' This is a convienient wrapper to add all the features in a single line.
#'
#' @param x results dataset from football data which with column names cleaned,
#' 
#' @export
#' @note 
#' Time independent features are features which can be derived from an 
#' observation (i.e. that match). Some features which could be useful are time
#' dependent e.g. average win % for past n matches depends on the teams past n
#' matches.
#'
#' @examples
#' See the function clean_results_fd for an implementation

add_independent_features <- function(x){
  
  features <- c("home_goals_half2",
                "away_goals_half2",
                "result",
                "result_half1",
                "result_half2",
                "over_under",
                "over_under_half1",
                "over_under_half2",
                "goal_difference",
                "goal_difference_half1",
                "goal_difference_half2",
                "ou25",
                "match_id")
  
  for(i in seq_along(1:length(features))){
    x <- add_independent_feature_fd(x, features[i])
  }
  
  return(x)
}

#' @title Add Independent Feature Football Data
#'
#' @description Add time independent features to football data results dataset. 
#' This is a convienient wrapper to add all the features in a single line.
#'
#' @param x results dataset from football data which with column names cleaned
#' @param feature see the function add_independent_features_fd for a list of 
#' possible string inputs. Incorrectly named inputs will warn and but not stop.
#' 
#' @export
#' @note 
#' Time independent features are features which can be derived from an 
#' observation (i.e. that match). Some features which could be useful are time
#' dependent e.g. average win % for past n matches depends on the teams past n
#' matches.
#'
#' @examples
#' See the function clean_results_fd for an implementation

add_independent_feature_fd <- function(x, feature){
  
  if(feature == "home_goals_half2"){
    x <- mutate(x, home_goals_half2 = home_goals - home_goals_half1)
    
  } else if(feature == "away_goals_half2"){
    x <- mutate(x, away_goals_half2 = away_goals - away_goals_half1)
    
  } else if(feature == "result"){
    x <- mutate(x, result = calculate_result(home_goals, away_goals))
    
  } else if(feature == "result_half1"){
    x <- x %>%
      mutate(result_half1 = calc_result(home_goals_half1, away_goals_half1))
    
  } else if(feature == "result_half2"){
    x <- x %>%
      mutate(result_half2 = calc_result(home_goals_half2, away_goals_half2))
    
  } else if(feature == "goal_difference"){
    x <-  mutate(x, goal_difference = home_goals - away_goals)
    
  } else if(feature == "goal_difference_half1"){
    x <-  mutate(x, goal_difference_half1 = home_goals_half1 - away_goals_half1)
    
  } else if(feature == "goal_difference_half2"){
    x <-  mutate(x, goal_difference_half2 = home_goals_half2 - away_goals_half2)
    
  # ou25 is over/under 2.5 goals market
    
  } else if(feature == "ou25"){
    x <- x %>%
      mutate(ou25 = if_else(home_goals + away_goals > 2.5, "over", "under"))
    
  } else if(feature == "match_id"){
    x <-  mutate(x, match_id = paste0(competition_id, ".", match_date, ".", 
                                      home_team, ".", away_team))
  } else {
    warning("unknown feature supplied")
  }
  
  return(x)
}
