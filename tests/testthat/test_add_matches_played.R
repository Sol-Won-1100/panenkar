
context("Add Matches Played")

test_that("add_matches_played works as expected", {
  
  file_results <- here::here() %>% paste0("/tests/testthat/sco_prem_1994_1995.rds")
  
  results <- file_results %>% 
    read_rds() %>%
    mutate(match_date = ymd(match_date),
           season_id = if_else(row_number() < 90, "1993_1994", season_id)) %>%
           add_matches_played()
    
  aberdeen_matches <- results %>% 
    pivot_longer(cols = c("home_team", "away_team"), names_to = "location", values_to = "team") %>%
    mutate(matches_played = if_else(location == "home_team", home_matches_played_season, 
                                    away_matches_played_season)) %>%
    filter(team == "Aberdeen") %>%
    select(match_date, season_id, team, location, matches_played)
  
  matches_played_9394 <- aberdeen_matches %>%
    filter(season_id == "1993_1994") %>%
    select(matches_played) %>%
    unlist()
  
  matches_played_9495 <- aberdeen_matches %>%
    filter(season_id == "1994_1995") %>%
    select(matches_played) %>%
    unlist()
  
  expect_equivalent(matches_played_9394, 1:length(matches_played_9394))
  expect_equivalent(matches_played_9495, 1:length(matches_played_9495))
})




