
#' @title Extract Teams by Season
#'
#' @description Returns a tibble with details of teams which were in a 
#' particular league by season
#'
#' @param x results database filtered so only a single competition id is present
#' 
#' @return tibble with details of teams and season_ids
#' @export

extract_teams_by_season <- function(x){
  
  if(length(unique(x$competition_id)) > 1){
    warning("Multiple different competition_id in x")
  }
  
  teams_by_season <- x %>%
    group_split(season_id) %>%
    map(select, home_team, away_team) %>%
    map(unlist) %>%
    map(unique)
  
  names(teams_by_season) <- unique(x$season_id)
  
  teams_lookup <- teams_by_season %>%
    map(tibble) %>%
    map(set_colnames, "team")
  
  teams_lookup <- list(.x = teams_lookup, .season_id = names(teams_lookup)) %>%
    pmap_dfr(mutate_season_id)
  
  return(teams_lookup)
}


#' @title Mutate Season ID
#'
#' @description Helper for add_promotion_relegation
#'
#' @param .x results database
#' @param .season_id season_id
#' 
#' @return .x with season_id variable
#' @note Helper function for extract_teams_by_season

mutate_season_id <- function (.x, .season_id) mutate(.x, season_id = .season_id)
  