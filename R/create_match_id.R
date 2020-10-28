
#' Create Match Id
#' 
#' Construct unique match id from other variables
#' 
#' @param competition_id unique competition id
#' @param match_date of class date
#' @param home_team name of home team
#' @param away_team name of away team
#' 
#' @return the match id

create_match_id <- function (competition_id, match_date, home_team, away_team) {
  

  if (is.Date(match_date) == FALSE) {
    
    stop ("match_date must be of class date")
    
  }
  
  if (home_team == away_team) {
    
    stop ("home_team must not equal away_team")
    
  }
  
  match_id <- match_date %>%
    as.character() %>%
    str_replace_all("-", "") %>%
    paste(competition_id, ., home_team, away_team, sep = "__") 
  
  return(match_id)
    
}

