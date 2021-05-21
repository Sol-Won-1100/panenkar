
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

create_match_id <- Vectorize (function (competition_id, match_date, home_team, away_team) {
  
  if (is.na(competition_id) | is.na(match_date) | is.na(home_team) | is.na(away_team)) {
    
    return(NA_character_)
    
  }
 
  if (is.Date(match_date) == FALSE) {
    
    stop ("match_date must be of class date")
    
  }
  if (sum(map2_lgl(home_team, away_team, magrittr::equals)) > 0) {
    
    warning ("home_team is equal to away_team")
    
  }
 
  match_id <- match_date %>%
    as.character() %>%
    str_replace_all("-", "") %>%
    paste(competition_id, ., home_team, away_team, sep = "__") 
  
  return(match_id)
    
})

