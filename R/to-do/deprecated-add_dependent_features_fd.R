
#' @title Add Dedependent Features Football Data
#'
#' @description Add time ependent features to football data results dataset. 
#' This is a convienient wrapper to add all the features in a single line.
#'
#' @param x results dataset from football data which with column names cleaned,
#' 
#' 
#' @note 
#' Time independent features are features which can be derived from an 
#' observation (i.e. that match). Some features which could be useful are time
#' dependent e.g. average win % for past n matches depends on the teams past n
#' matches.

add_dependent_features_fd <- function(x){
  
  features <- c("match_numbers_season", "promotion_relegation", "add_spell")
  
  for (i in seq_along(1:length(features))){
    print(i)
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
    competition_ids <- metadata$competition_id
    
    x <- map_dfr(competition_ids, add_promotion_relegation, database_results = x, .metadata = metadata)

  } else if (feature == "add_spell") {
    
    metadata <- get_metadata(output_format = "tibble")

    teams_with_spell <- map_dfr(metadata$competition_id, add_spell, database_results = x)
  
   
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





#' @title Add Promotion Relegation
#'
#' @description Adds in indicator variables for teams newly promoted or 
#' relegated to a league for that season. 
#'
#' @param x results database
# database_results <- wd$data_clean %>%
#   paste0("database-results.rds") %>%
#   read_rds()
# 
# .metadata <- get_metadata(output_format = "tibble")
# 
# .competition_id = "eng_champ"

add_promotion_relegation <- function(.competition_id, database_results, .metadata){
  
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
    
    competition_id_above <- extract_element(metadata_above, 1, "competition_id")
    
  } else {
    
    competition_id_above <- NA
    
  }

  # Extract from results data
  
  x_competition <- filter(database_results, competition_id == .competition_id)
  
  teams_lookup <- x_competition %>%
    extract_teams_by_season() %>%
    mutate(promoted_into = NA_real_, relegated_into = NA_real_, promoted_from = NA_real_, relegated_from = NA_real_)
  
  teams_lookup_split <- group_split(teams_lookup, season_id)
  
  # Promoted teams (can infer teams relegated from above by elimination)
  
  if (!is.na(competition_id_above)) {
    
    teams_lookup_above <- database_results %>%
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

    if (!is.na(competition_id_above)) {
      
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
  
  # We use mutate and then select to drop variables rather than just rename. This is because the variables may already
  # be present in the dataset from previous runs so this overwrites them and we dont fling up a duplicate name error
  
  x_competition <- x_competition %>% 
    left_join(promotion_relegation_lookup, by = c("home_team" = "team", "competition_id", "season_id")) %>%
    mutate(home_promoted_into = promoted_into, 
           home_relegated_into = relegated_into, 
           home_promoted_from = promoted_from,
           home_relegated_from = relegated_from) %>%
    select(-promoted_into, -relegated_into, -promoted_from, -relegated_from) %>%
    left_join(promotion_relegation_lookup, by = c("away_team" = "team", "competition_id", "season_id")) %>%
    mutate(away_promoted_into = promoted_into, 
           away_relegated_into = relegated_into, 
           away_promoted_from = promoted_from,
           away_relegated_from = relegated_from) %>%
    select(-promoted_into, -relegated_into, -promoted_from, -relegated_from)

  return(x_competition)
  
}




#' @title Add Spell
#'
#' @description Adds in which spell, since the start of the dataset, the teams are currently in
#'
#' @param .competition_id competition_id to construct spells dataset for
#' @param database_results results database
#' 
#' @note A helper function for add_dependent_feature, see above
#' @return 

add_spell <- function(.competition_id, database_results) {
  
  x_split_season_id <- database_results %>% 
    filter(competition_id == .competition_id) %>%
    group_split(season_id)

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
  
  
  # Leave off from here - you have a problem in this segment of code methinks
  
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
#' @param database_results results database
#' 
#' @note A helper function for add_spell, see above
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



