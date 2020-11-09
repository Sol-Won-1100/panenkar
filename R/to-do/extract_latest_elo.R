
#' @title Extract Latest ELO
#'
#' @description Given extract the latest elo ratings for a specified competition
#'
#' @param .competition_d The competition id
#' @param results_elo results tibble with elo ratings included
#' @param latest_season_teams default is TRUE, if TRUE only teams in the latest
#' season are returned
#' @param sorted do you want to sort the elo ratings from largest to smallest?
#'       
#' 

extract_latest_elo <- function(.competition_id, results_elo, 
                               latest_season_teams = TRUE, sorted = TRUE){
  
  results_elo_competition <- results_elo %>% 
    filter(competition_id == .competition_id) %>%
    arrange(desc(match_date))
  
  team <- results_elo_competition %>%
    select(home_team, away_team) %>%
    unlist() %>%
    unique()
  
  # Pull each teams latest home and away matches and elos then select the most
  # recent score
  
  results_elo_competition_latest_home <- results_elo_competition %>%
    distinct(home_team, .keep_all = TRUE) %>%
    mutate(home_match_date = match_date, team = home_team) %>%
    select(team, home_match_date, elo_home_post)
  
  results_elo_competition_latest_away <- results_elo_competition %>%
    distinct(away_team, .keep_all = TRUE) %>%
    mutate(away_match_date = match_date, team = away_team) %>%
    select(team, away_match_date, elo_away_post)
  
  elo_record <- tibble(team) %>%
    left_join(results_elo_competition_latest_home, by = "team") %>%
    left_join(results_elo_competition_latest_away, by = "team") %>%
    mutate(elo = if_else(home_match_date > away_match_date, elo_home_post, 
                         elo_away_post)) %>%
    select(team, elo)
  
  if(latest_season_teams == TRUE){
    latest_season <- unlist(results_elo_competition[1, "season_id"])
    
    teams_latest <- results_elo_competition %>%
      filter(season_id == latest_season) %>%
      select(home_team, away_team) %>%
      unlist() %>%
      unique()
    
    elo_record <- filter(elo_record, team %in% teams_latest)

  }
  
  if(sorted == TRUE){
    elo_record <- arrange(elo_record, desc(elo))
  }
  
  return(elo_record)
}

