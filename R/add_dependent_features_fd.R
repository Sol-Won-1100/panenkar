
#' @title Add Dedependent Features Football Data
#'
#' @description Add time ependent features to football data results dataset. 
#' This is a convienient wrapper to add all the features in a single line.
#'
#' @param x results dataset from football data which with column names cleaned,
#' 
#' @export
#' @note 
#' Time independent features are features which can be derived from an 
#' observation (i.e. that match). Some features which could be useful are time
#' dependent e.g. average win % for past n matches depends on the teams past n
#' matches.

add_dependent_features_fd <- function(x){
  
  features <- c("match_numbers_season", "promotion_relegation", "add_spell")
  
  for(i in seq_along(1:length(features))){

    x <- add_dependent_feature_fd(x, features[i])

  }
  
  return(x)
}


#' @title Add Dependent Features Football Data
#'
#' @description Add time independent features to football data results dataset. 
#' This is a convienient wrapper to add all the features in a single line.
#' 
#' @param x results database
#' @param feature see the function add_dependent_features_fd for a list of 
#' possible string inputs. Incorrectly named inputs will warn and but not stop.
#'
#' @param x results database

add_dependent_feature_fd <- function(x, feature) {
  
  if (feature == "match_numbers_season") {
    
    x <- add_match_numbers_season(x)
    
  } else if(feature == "promotion_relegation") {
    
    metadata <- get_metadata(output_format = "tibble")
    competition_ids <- metdata$competition_id
    
    x_condensed <- select(x, competition_id, season_id, home_team, away_team, match_id)
    
    promotion_relegation <- map_dfr(competition_ids, add_promotion_relegation, .x = x_condensed, .metadata = metadata)
    x <- left_join(x, promotion_relegation, by = "match_id")
    

  } else if (feature == "add_spell") {
    
    metadata <- get_metadata(output_format = "tibble")

    teams_with_spell <- map_dfr(metadata$competition_id, add_spell, x)
    
    x <- x %>%
      left_join(teams_with_spell, by = c("home_team" = "team", "season_id", "competition_id")) %>%
      rename(home_team_spell = spell) %>%
      left_join(teams_with_spell, by = c("away_team" = "team", "season_id", "competition_id")) %>%
      rename(away_team_spell = spell)
    
    
  } else {
    
    warning(paste0("unknown feature supplied ", feature))
    
  }
  
  return(x) 
  
}


#' @title Add Match Numbers Season
#'
#' @description Adds in the match number in the season for 
#' the home team and the away team to the results database. For consistent 
#' syntax for adding features this function is a helper called by 
#' add_dependent_feature
#'
#' @param x results database

add_match_numbers_season <- function(x){
  
  x_subset_with_match_number <- x %>%
    arrange(match_id) %>%
    select(match_id, season_id, home_team, away_team) %>%
    pivot_longer(cols = home_team:away_team, names_to = "location", 
                 values_to = "team") %>%
    group_by(season_id, team) %>%
    mutate(match_number_season = 1:n()) %>%
    ungroup()
  
  x_subset_with_match_number_home <- x_subset_with_match_number %>% 
    filter(location == "home_team") %>%
    rename(home_match_number_season = match_number_season) %>%
    select(match_id, team, home_match_number_season)
  
  x_subset_with_match_number_away <- x_subset_with_match_number %>% 
    filter(location == "away_team") %>%
    rename(away_match_number_season = match_number_season) %>%
    select(match_id, team, away_match_number_season)
  
  home_join <- c("match_id", "home_team" = "team")
  away_join <- c("match_id", "away_team" = "team")
  
  x_with_match_number_season <- x %>%
    left_join(x_subset_with_match_number_home, by = home_join) %>%
    left_join(x_subset_with_match_number_away, by = away_join)
  
  return(x_with_match_number_season)
}


#' @title Add Promotion Relegation
#'
#' @description Adds in indicator variables for teams newly promoted or 
#' relegated to a league for that season. 
#'
#' @param x results database
# .x <- wd$data_clean %>%
#   paste0("database-results.rds") %>%
#   read_rds()
# 
# .metadata <- get_metadata(output_format = "tibble")
# 
# .competition_id = "eng_champ"


add_promotion_relegation <- function(.competition_id, .x, .metadata){
  
  # Pull out the required competition data and for competitions above and below in the league system for that particular 
  # league from metadata
  
  
  
  row_competition_id <- which(.metadata$competition_id == .competition_id)
  
  tier <- extract_element(.metadata, row_competition_id, competition_tier) 
  country <- extract_element(.metadata, row_competition_id, competition_region) 
  
  # League in tier below 
  
  metadata_below <- filter(.metadata, competition_tier == tier + 1, competition_region == country)
  
  if (nrow(metadata_below) > 0) {
    
    competition_id_below <- extract_element(metadata_below, 1, competition_id)
    
  } else {
    
    competition_id_below <- NA
    
  }
  
  metadata_above <- filter(.metadata, competition_tier == tier - 1, competition_region == country)
  
  if (nrow(metadata_above) > 0) {
    
    competition_id_above <- extract_element(metadata_above, 1, competition_id)
    
  } else {
    
    competition_id_above <- NA
    
  }
  
  # Extract from results data
  x_competition <- filter(.x, competition_id == .competition_id)
  
  teams_lookup <- x_competition %>%
    extract_teams_by_season() %>%
    mutate(promoted_into = NA_real_, relegated_into = NA_real_, promoted_from = NA_real_, relegated_from = NA_real_)
  
  teams_lookup_split <- group_split(teams_lookup, season_id)
  
  # Promoted teams (can infer teams relegated from above by elimination)
  
  if (!is.na(competition_id_above)) {
    
    teams_lookup_above <- .x %>%
      filter(competition_id == competition_id_above) %>%
      extract_teams_by_season()
    
    teams_lookup_above_split <- teams_lookup_above %>%
      group_split(season_id) %>%
      set_names(unique(teams_lookup_above$season_id))
    
  }  
  
  ## Derive the into variables
  
  # If team was in the league last season then 0 for promoted into relegated
  # into
  
  # If team not in league last season and top tier league then promoted into
  
  # Now only look at tier above for non-top tier competitions. If team not in 
  # league last season and in tier above last season then relegated into
  
  for (i in 2:length(teams_lookup_split)) {
    
    teams_previous <- teams_lookup_split[[i - 1]]$team
    
    if (!is.na(competition_id_above)) {
      
      teams_previous_above <- teams_lookup_above_split[[i - 1]]$team
      
    } else {
      
      teams_previous_above <- NA
      
    }
    
    teams_lookup_split[[i]] <- teams_lookup_split[[i]] %>% 
      mutate(promoted_into = if_else(team %in% teams_previous, 0, promoted_into),
             relegated_into = promoted_into,
             promoted_into = if_else(is.na(promoted_into) & tier == 1, 1,  promoted_into),
             relegated_into = if_else(is.na(relegated_into) & tier == 1, 0, relegated_into),
             relegated_into = if_else(is.na(relegated_into) & team %in%  teams_previous_above, 1, relegated_into),
             promoted_into = if_else(is.na(promoted_into) & relegated_into == 1, 0, promoted_into),
             relegated_into = if_else(is.na(relegated_into), 0, relegated_into),
             promoted_into = if_else(is.na(promoted_into), 1, promoted_into))
    
  }
  
  ## Derive the from variables
  
  for (i in 1:(length(teams_lookup_split) - 1)) {
    
    teams_next <- teams_lookup_split[[i + 1]]$team
    
    if (!is.na(competition_id)) {
      
      teams_next_above <- teams_lookup_above_split[[i + 1]]$team
      
    } else {
      
      teams_next_above <- NA
      
    }
    
    teams_lookup_split[[i]] <- teams_lookup_split[[i]] %>%
      mutate(promoted_from = case_when(team %in% teams_next ~ 0,
                                       team %in% teams_next_above ~ 1,
                                       TRUE ~ 0),
             relegated_from = case_when(team %in% teams_next ~ 0,
                                        promoted_from == 1 ~ 0,
                                        TRUE ~ 1))
    
  }
  
  promotion_relegation_lookup <- teams_lookup_split %>%
    bind_rows() %>%
    mutate(competition_id = .competition_id)
  
  x_competition <- x_competition %>% 
    left_join(promotion_relegation_lookup, by = c("home_team" = "team", "competition_id", "season_id")) %>%
    rename(home_promoted_into = promoted_into, 
           home_relegated_into = relegated_into, 
           home_promoted_from = promoted_from,
           home_relegated_from = relegated_from) %>%
    left_join(promotion_relegation_lookup, by = c("away_team" = "team", "competition_id", "season_id")) %>%
    rename(away_promoted_into = promoted_into, 
           away_relegated_into = relegated_into, 
           away_promoted_from = promoted_from,
           away_relegated_from = relegated_from) %>%
    select(-competition_id, -season_id, -home_team, -away_team)
  
  return(x_competition)
  
}




#' @title Add Spell
#'
#' @description Adds in which spell, since the start of the dataset, the teams are currently in
#'
#' @param .competition_id competition_id to construct spells dataset for
#' @param .x results database
#' 
#' @note A helper function for add_dependent_feature, see above
#' @return 

add_spell <- function(.competition_id, .x) {

  x_split_season_id <- .x %>% 
    filter(competition_id == .competition_id) %>%
    group_split(, season_id)
  
  season_ids <- x_split_season_id %>% map("season_id") %>% map(1) %>% unlist()
  
  teams_by_season <- x_split_season_id %>%
    map(~select(., home_team, away_team)) %>%
    map(unlist) %>%
    map(unique) %>%
    map(tibble) %>%
    map2(season_ids, ~mutate(.x, season_id = .y)) %>%
    map(set_colnames, c("team", "season_id")) %>% 
    bind_rows() %>%
    mutate(spell = 1) %>%
    pivot_wider(names_from = "season_id", values_from = "spell")

  teams_by_season[is.na(teams_by_season)] <- 0

  teams_with_spell <- teams_by_season %>%
    pivot_longer(cols = all_of(season_ids), names_to = "season_id", values_to = "present") %>%
    group_by(team) %>%
    mutate(cum_present = cumsum(present), spell = 1) %>%
    filter(cum_present > 0) %>%
    group_split() %>%
    map(calc_spell) %>%
    map(select, -present, -cum_present) %>%
    bind_rows() %>%
    mutate(competition_id = .competition_id)
  
  return(teams_with_spell)

}


#' @title Calculate Spell
#'
#' @description Adds in which spell, since the start of the dataset, the teams are currently in
#'
#' @param .competition_id competition_id to construct spells dataset for
#' @param .x results database
#' 
#' @note A helper function for add_dependent_feature, see above
#' @return 

calc_spell <- function (x) {

  if (nrow(x) > 1) {
    
    for (i in 2:nrow(x)) {
      
      present_current <- unlist(x[i, "present"])
      present_previous <- unlist(x[i - 1, "present"])
      
      if (present_current == 1 & present_previous == 0) {
        
        x[i:nrow(x), "spell"] <- unlist(x[i - 1, "spell"]) + 1
        
      }
      
    }
    
  }
  
  return(x)
  
}



