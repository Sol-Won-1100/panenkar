
#' @title Calculate Season ELO
#'
#' @description Calculate ELO ratings for a single season
#'
#' @param results set of results to calculate ELO rating for
#' @param elo_record starting ELO ratings for each team in the results data
#' 
#' @note 
#'       
#' @export
#'
#' @examples

calc_season_elo <- function(results, elo_record){
  
  results <- mutate(results, elo_home_pre = NA_real_, elo_away_pre = NA_real_,
                    elo_home_post = NA_real_, elo_away_post = NA_real_)
  
  for(j in 1:nrow(results)){
    
    # Exctract match details - don't exctract date into a vector because it
    # changes the type so we will access it using the $ operator
    
    match <- results %>% slice(j)
    
    match_home_team <- match %>% select(home_team) %>% unlist()
    match_away_team <- match %>% select(away_team) %>% unlist()
    match_probs_home <- match %>% select(home_prob) %>% unlist()
    match_probs_draw <- match %>% select(draw_prob) %>% unlist()
    match_probs_away <- match %>% select(away_prob) %>% unlist()
    
    # Lookup teams elos from the record
    
    match_elo_home_pre <- elo_record %>% 
      filter(team == match_home_team) %>%
      select(elo) %>%
      unlist()
    
    match_elo_away_pre <- elo_record %>% 
      filter(team == match_away_team) %>%
      select(elo) %>%
      unlist()
    
    # Update the pre match elos in the season results
    
    results <- results %>%
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
      match_elos_post <- update_elo(match_elo_home_pre, match_elo_away_pre, 
                                    match_probs_home, match_probs_draw, 
                                    match_probs_away) %>%
        unlist()
    }
    
    # Update the post match elos in the season results and amend elo record
    
    results <- results %>%
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
  
  return(list(results = results, elo_record = elo_record))
}
