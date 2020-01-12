
calc_season_elo_odds <- function(results_season, elo_record){
  
  
  for(j in 1:nrow(results_season)){
    
    # Exctract match details - don't exctract date into a vector because it
    # changes the type so we will access it using the $ operator
    
    match <- results_season %>% slice(j)
    
    match_home_team <- match %>% select(home_team) %>% unlist()
    match_away_team <- match %>% select(away_team) %>% unlist()
    match_probs_home <- match %>% select(probs_home_no_margin) %>% unlist()
    match_probs_draw <- match %>% select(probs_draw_no_margin) %>% unlist()
    match_probs_away <- match %>% select(probs_away_no_margin) %>% unlist()
    
    # Lookup teams elos from the record
    
    match_elo_home_pre <- elo_record %>% 
      filter(team == match_home_team) %>%
      select(elo) %>%
      unlist()
    
    match_elo_away_pre <- elo_record %>% 
      filter(team == match_away_team) %>%
      select(elo) %>%
      unlist()
    
    # Update the pre match elos in the season results_season
    
    results_season <- results_season %>%
      mutate(elo_home_pre = if_else(row_number() == j, 
                                    match_elo_home_pre,
                                    elo_home_pre),
             elo_away_pre = if_else(row_number() == j, 
                                    match_elo_away_pre,
                                    elo_away_pre))
    
    # Calculate the new elos
    
    if(is.na(match_probs_home) | is.na(match_probs_draw) | is.na(match_probs_away)){
      match_elos_post <- c(match_elo_home_pre, match_elo_away_pre)
    } else {
      match_elos_post <- update_elo_odds(match_elo_home_pre, match_elo_away_pre, 
                                         match_probs_home, match_probs_draw, 
                                         match_probs_away)
    }
    
    # Update the post match elos in the season results_season and amend elo record
    
    results_season <- results_season %>%
      mutate(elo_home_post = if_else(row_number() == j, 
                                     match_elos_post[1],
                                     elo_home_post),
             elo_away_post = if_else(row_number() == j, 
                                     match_elos_post[2],
                                     elo_away_post))
    elo_record <- elo_record %>%
      mutate(elo = if_else(team == match_home_team, match_elos_post[1], elo),
             elo = if_else(team == match_away_team, match_elos_post[2], elo))
    
    
  }
  
  return(list(results_season = results_season, elo_record = elo_record))
}






