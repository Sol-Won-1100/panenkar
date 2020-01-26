
#' @title Calculate Competition ELO
#'
#' @description Calculate ELO ratings for a competition, over multiple seasons
#' if necessary
#'
#' @param .competition_id see get_metadata for format of competition ids
#' @param results standard results tibble but with 3 columns for home_prob,
#' draw_prob and away_prob. These probabilities should be implied probabilities
#' derived from bookmakers odds.
#' 
#' @export


calc_competition_elo <- function(.competition_id, results, 
                                 message_user = FALSE){
  
  metadata <- get_metadata(.competition_id)
  
  results_competition <- results %>%
    select(match_id, competition_id, season_id, match_date, 
           home_team:match_result_half2, home_prob:away_prob) %>%
    filter(competition_id == .competition_id) %>%
    arrange(match_date) 
  
  # Need information on leagues which have promotion or relegation. But we only
  # need to find the teams which were promoted because by definition the ones
  # relegated must be the other ones. This also solves the problem of teams 
  # getting relegated down multiple leagues e.g. Rangers because with this 
  # approach we don't care we just know they are gone.
  
  competition_promoted_into <- metadata$competition_promoted_into
  
  if(!is.na(competition_promoted_into)){
    results_competition_promoted <- results %>%
      filter(competition_id == competition_promoted_into)
  }
  
  all_teams <- unique(c(results_competition$home_team, 
                        results_competition$away_team))
  
  elo_record <- tibble(team = all_teams, elo = 1000)
  
  season_ids <- unique(results_competition$season_id)
  
  competition_promoted_into <- metadata$competition_promoted_into
  
  results_competition_elos <- list() # To store the results
  
  for(i in seq_along(season_ids)){
    
    results_competition_season <- filter(results_competition, 
                                         season_id == season_ids[i])
    
    # What to identify which teams have left the league and which have joined
    # from last season and establish if they were relegated or promoted. This 
    # will determine starting ELOs for entry to a league
    
    if(i > 1){
      
      current_teams <- unique(c(results_competition_season$home_team, 
                                results_competition_season$away_team))
      previous_teams <- unique(c(results_competition_season_previous$home_team, 
                                 results_competition_season_previous$away_team))
      
      teams_left <- previous_teams[!(previous_teams %in% current_teams)]
      teams_new <- current_teams[!(current_teams %in% previous_teams)]
      
      # If league has no league on record which has promotion then assume it is
      # a top tier league
      
      if(is.na(metadata$competition_promoted_into)){
        teams_relegated_from <- teams_left
        teams_promoted_from <- c()
        
        teams_promoted_to <- teams_new
        teams_relegated_to <- c()
        
      } else {
        results_competition_above <- results_competition_promoted %>%
          filter(competition_id ==  competition_promoted_into,
                 season_id == season_ids[i]) 
        
        teams_in_above_tier <- results_competition_above %>%
          select(home_team, away_team) %>%
          unlist() %>%
          unique()
        
        results_competition_above_previous <- results_competition_promoted %>%
          filter(competition_id == competition_promoted_into,
                 season_id == season_ids[i - 1])
        
        teams_in_above_tier_previous <- results_competition_above_previous %>%
          select(home_team, away_team) %>%
          unlist() %>%
          unique()
        
        teams_promoted_from <- teams_left[teams_left %in% teams_in_above_tier]
        teams_relegated_from <- teams_left[!(teams_left %in% teams_promoted_from)]
        
        teams_relegated_to <- teams_new[teams_new %in% teams_in_above_tier_previous]
        teams_promoted_to <- teams_new[!(teams_new %in% teams_relegated_to)]
      }
      
      if(length(teams_relegated_to) > 0){
        teams_promoted_from_average_elo <- elo_record %>%
          filter(team %in% teams_promoted_from) %>%
          select(elo) %>%
          unlist() %>%
          mean()
        
        elo_record <- mutate(elo_record, 
                             elo = if_else(team %in% teams_relegated_to, 
                                           teams_promoted_from_average_elo,
                                           elo))
      }
      
      if(length(teams_promoted_to) > 0){
        teams_relegated_from_average_elo <- elo_record %>%
          filter(team %in% teams_relegated_from) %>%
          select(elo) %>%
          unlist() %>%
          mean()
        
        elo_record <- mutate(elo_record, 
                             elo = if_else(team %in% teams_promoted_to,
                                           teams_relegated_from_average_elo,
                                           elo))
        
      }
    }
    
    results_competition_season_elos <- calc_season_elo(results_competition_season, 
                                                       elo_record)
    
    # Store
    results_competition_elos[[i]] <- results_competition_season_elos$results
    elo_record <- results_competition_season_elos$elo_record
    results_competition_season_previous <- results_competition_season
    
  }
  
  if(message_user == TRUE){
    .competition_id %>% paste0(" complete at ", Sys.time()) %>% message()
  }
  
  return(bind_rows(results_competition_elos))
}
