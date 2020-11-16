
# Add matches played  by each team for each season to standard results tibble.

add_matches_played <- function(results){
  
  expected_cols <- c("home_team", "away_team", "season_id")
  
  check_error_results(results, expected_cols)
  
  results_subset_with_matches_played <- results %>%
    pivot_longer(cols = home_team:away_team, names_to = "location", values_to = "team") %>%
    group_by(season_id, team) %>%
    mutate(matches_played_season = 1:n()) %>%
    ungroup()
  
  results_subset_with_matches_played_home <- filter(results_subset_with_matches_played, location == "home_team")
  results_subset_with_matches_played_away <- filter(results_subset_with_matches_played, location == "away_team") 
  
  results_with_matches_played_season <- results %>%
    mutate(home_matches_played_season = results_subset_with_matches_played_home$matches_played_season,
           away_matches_played_season = results_subset_with_matches_played_away$matches_played_season)
  
  return(results_with_matches_played_season)
  
}
