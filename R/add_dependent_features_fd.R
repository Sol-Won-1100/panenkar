
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
  
  features <- c("match_numbers_season", "promotion_relegation")
  
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

add_dependent_feature_fd <- function(x, feature){

  if(feature == "match_numbers_season"){
    x <- add_match_numbers_season(x)
    
  } else if(feature == "promotion_relegation"){
    
    metadata <- get_metadata(output_format = "tibble")
    
    x <- metadata$competition_id %>%
      map_dfr(add_promotion_relegation, .x = x, .metadata = metadata)
    
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

add_promotion_relegation <- function(.competition_id, .x, .metadata){
 
  # Pull out the required competition data and for competitions above and below
  # in the league system for that particular league from metadata
  print(.competition_id)
  row_competition_id <- which(.metadata$competition_id == .competition_id)
  
  tier <- extract_element(.metadata, row_competition_id, competition_tier) 
  country <- extract_element(.metadata, row_competition_id, competition_region) 
  
  # League in tier below 
  
  metadata_below <- .metadata %>% 
    filter(competition_tier == tier + 1, competition_region == country)
  
  if(nrow(metadata_below) > 0){
    competition_id_below <- extract_element(metadata_below, 1, competition_id)
    
  } else {
    competition_id_below <- NA
    
  }
  
  metadata_above <- .metadata %>% 
    filter(competition_tier == tier - 1, competition_region == country)
  
  if(nrow(metadata_above) > 0){
    competition_id_above <- extract_element(metadata_above, 1, competition_id)
    
  } else {
    competition_id_above <- NA
    
  }
  
  # Extract from results data
  x_competition <- filter(.x, competition_id == .competition_id)
  
  teams_lookup <- x_competition %>%
    extract_teams_by_season() %>%
    mutate(promoted_into = NA_real_, relegated_into = NA_real_)
  
  teams_lookup_split <- group_split(teams_lookup, season_id)
  
  # Promoted teams (can infer teams relegated from above by elimination)
  
  if(!is.na(competition_id_above)){
    teams_lookup_above <- .x %>%
      filter(competition_id == competition_id_above) %>%
      extract_teams_by_season()
    
    teams_lookup_above_split <- teams_lookup_above %>%
      group_split(season_id) %>%
      set_names(unique(teams_lookup_above$season_id))
  }  
  
  # Logic:
  
  # If team was in the league last season then 0 for promoted into relegated
  # into
  
  # If team not in league last season and top tier league then promoted into
  
  # Now only look at tier above for non-top tier competitions. If team not in 
  # league last season and in tier above last season then relegated into
  
  for(i in 2:length(teams_lookup_split)){
    teams_previous <- teams_lookup_split[[i - 1]]$team
    
    if(!is.na(competition_id_above)){
      teams_previous_above <- teams_lookup_above_split[[i - 1]]$team
    } else {
      teams_previous_above <- NA
    }
    
    teams_lookup_split[[i]] <- teams_lookup_split[[i]] %>% 
      mutate(
        # Present last season
        promoted_into = if_else(team %in% teams_previous, 0, promoted_into),
        relegated_into = promoted_into,
        
        # Top tier and not present so must be promoted
        promoted_into = if_else(is.na(promoted_into) & tier == 1, 1, 
                                promoted_into),
        relegated_into = if_else(is.na(relegated_into) & tier == 1, 0, 
                                 relegated_into),
        
        # Not top tier and not present so check competition_above
        relegated_into = if_else(is.na(relegated_into) & team %in% 
                                   teams_previous_above, 1, relegated_into),
        promoted_into = if_else(is.na(promoted_into) & relegated_into == 1, 0,
                                promoted_into),
        
        # Not top tier and not present and checked competition_above infer below
        relegated_into = if_else(is.na(relegated_into), 0, relegated_into),
        promoted_into = if_else(is.na(promoted_into), 1, promoted_into)
      )
  }
  
  promotion_relegation_lookup <- teams_lookup_split %>%
    bind_rows() %>%
    mutate(competition_id = .competition_id)
  
  x_competition <- x_competition %>% 
    left_join(promotion_relegation_lookup, 
              by = c("home_team" = "team", "competition_id", "season_id")) %>%
    rename(home_promoted_into = promoted_into, 
           home_relegated_into = relegated_into) %>%
    left_join(promotion_relegation_lookup,
              by = c("away_team" = "team", "competition_id", "season_id")) %>%
    rename(away_promoted_into = promoted_into,
           away_relegated_into = relegated_into)
  print(x_competition)
  return(x_competition)
}

#' @title Mutate Season ID
#'
#' @description Helper for add_promotion_relegation
#'
#' @param .x results database
#' @param .season_id season_id
#' 
#' @return .x with season_id variable

mutate_season_id <- function(.x, .season_id){
  
  mutate(.x, season_id = .season_id)
  
}

#' @title Add Spell
#'
#' @description Adds in which spell, since the start of the dataset, the teams
#' are currently in
#'
#' @param x results database

add_spell <- function(x){
  
}


