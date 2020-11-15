
#' @title Add Matches Played
#' @description Add matches played by each team for each season
#' @param x results tibble with cols 'home_team', 'away_team' and 'season_id'
#' @return 
#' @rdname add_matches_played
#' @export 
#' 

add_matches_played <- function(x){
  
  x_subset_with_matches_played <- x %>%
    pivot_longer(cols = home_team:away_team, names_to = "location", values_to = "team") %>%
    group_by(season_id, team) %>%
    mutate(matches_played_season = 1:n()) %>%
    ungroup()
  
  x_subset_with_matches_played_home <- filter(x_subset_with_matches_played, location == "home_team")
  x_subset_with_matches_played_away <- filter(x_subset_with_matches_played, location == "away_team") 
  
  x_with_matches_played_season <- x %>%
    mutate(home_matches_played_season = x_subset_with_matches_played_home$matches_played_season,
           away_matches_played_season = x_subset_with_matches_played_away$matches_played_season)
  
  return(x_with_matches_played_season)
  
}
