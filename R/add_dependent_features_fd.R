
#' @title Add Dedependent Features Football Data
#'
#' @description Add time ependent features to football data results dataset. 
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

add_dependent_features_fd <- function(x){
  
  features <- c("match_numbers_season")
  
  for(i in seq_along(1:length(features))){
    x <- add_dependent_feature_fd(x, features[i])
  }
  
  return(x)
}


#' @title Add Dependent Features Football Data
#'
#' @description Add time independent features to football data results dataset. 
#' This is a convienient wrapper to add all the features in a single line.
#' 
#' @param feature see the function add_dependent_features_fd for a list of 
#' possible string inputs. Incorrectly named inputs will warn and but not stop.
#'
#' @param x results database

add_dependent_feature_fd <- function(x, feature){
  
  if(feature == "match_numbers_season"){
    x <- add_match_numbers_season(x)
    
  } else {
    warning(paste0("unknown feature supplied ", feature))
    
  }
 
  return(x) 
}


#' @title Add Match Numbers Season
#'
#' @description Adds in the match number in the season for 
#' the home team and the away team to the results database. For consistent 
#' syntax for adding features this function is a helper called by 
#' add_dependent_feature
#'
#' @param x results database

add_match_numbers_season <- function(x){
  
  x_subset_with_match_number <- x %>%
    arrange(match_id) %>%
    select(match_id, season_id, home_team, away_team) %>%
    pivot_longer(cols = home_team:away_team, names_to = "location", 
                 values_to = "team") %>%
    group_by(season_id, team) %>%
    mutate(match_number_season = 1:n()) %>%
    ungroup()
  
  x_subset_with_match_number_home <- x_subset_with_match_number %>% 
    filter(location == "home_team") %>%
    rename(home_match_number_season = match_number_season) %>%
    select(match_id, team, home_match_number_season)
  
  x_subset_with_match_number_away <- x_subset_with_match_number %>% 
    filter(location == "away_team") %>%
    rename(away_match_number_season = match_number_season) %>%
    select(match_id, team, away_match_number_season)
  
  home_join <- c("match_id", "home_team" = "team")
  away_join <- c("match_id", "away_team" = "team")
  
  x_with_match_number_season <- x %>%
    left_join(x_subset_with_match_number_home, by = home_join) %>%
    left_join(x_subset_with_match_number_away, by = away_join)
  
  return(x_with_match_number_season)
}

