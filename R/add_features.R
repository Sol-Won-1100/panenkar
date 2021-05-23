
#' @title Add COVID empty stadiums
#'
#' @description Add TRUE or FALSE if stadiums are empty, mainly due to COVID, as this will change the home advantage
#' dynamic
#'
#' @param results results tibble
#' @return results tibble 
#' @note dates are taken from metadata to determine if stadiums are empty or partially empty. Through time I will add
#' functionality to note if certain matches were played behind closed doors
#'
#' @export

add_empty_stadiums <- function(results, metadata_competitions = load_metadata()$competitions) {
  
  # Firstly extract where match ids when stadium was empty are manually specified
  
  match_ids_extracted_empty_stadium <- metadata_competitions %>%
    select(match_ids_empty_stadium) %>%
    filter(!is.na(match_ids_empty_stadium)) %>%
    unlist() %>%
    str_split(";") %>%
    unlist() %>%
    str_replace_all('\"', "") %>%
    str_replace_all("\'", "") %>%
    str_trim()

  match_ids_extracted_partial_empty_stadium <- metadata_competitions %>%
      select(match_ids_partial_empty_stadium) %>%
      filter(!is.na(match_ids_partial_empty_stadium)) %>%
      unlist() %>%
      str_split(";") %>%
      unlist() %>%
      str_replace_all('\"', "") %>%
      str_replace_all("\'", "") %>%
    str_trim()

  metadata_competitions %>%
    select(competition_id, start_date_covid_empty_stadium, end_date_covid_empty_stadium, match_ids_empty_stadium,
           start_date_covid_partial_empty_stadium, end_date_covid_partial_empty_stadium, 
           match_ids_partial_empty_stadium) %>%
    left_join(results, ., by = "competition_id") %>%
    mutate(
      start_date_covid_empty_stadium = case_when(
        is.na(start_date_covid_empty_stadium) ~ max(match_date) + 1, 
        TRUE ~ start_date_covid_empty_stadium),
      
      end_date_covid_empty_stadium = case_when(
        is.na(end_date_covid_empty_stadium) ~ max(match_date) + 1, 
        TRUE ~ end_date_covid_empty_stadium),
      
      start_date_covid_partial_empty_stadium = case_when(
        is.na(start_date_covid_partial_empty_stadium) ~ max(match_date) + 1, 
        TRUE ~ start_date_covid_partial_empty_stadium),
      
      end_date_covid_partial_empty_stadium = case_when(
        is.na(end_date_covid_partial_empty_stadium) ~ max(match_date) + 1, 
        TRUE ~ end_date_covid_partial_empty_stadium),
      
      is_empty_stadium = case_when(
        match_date >= start_date_covid_empty_stadium & match_date <= end_date_covid_empty_stadium ~ TRUE,
        match_id %in% match_ids_extracted_empty_stadium ~ TRUE,
        TRUE ~ FALSE
      ),
      
      is_partial_empty_stadium = case_when(
        match_date >= start_date_covid_partial_empty_stadium & match_date <= end_date_covid_partial_empty_stadium ~ TRUE,
        match_id %in% match_ids_extracted_partial_empty_stadium ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    select(-start_date_covid_empty_stadium, -end_date_covid_empty_stadium, -start_date_covid_partial_empty_stadium,
           -end_date_covid_partial_empty_stadium, -match_ids_empty_stadium, -match_ids_partial_empty_stadium)
}



#' @title Add Matches Played
#' @description Add matches played that season for each team
#' @param results standard results tibble
#' @return results tibble with home_matches_played_season and away_matches_played_season
#' @rdname add_matches_played
#' @export 

add_matches_played <- function(results, rows = NA){
  
  expected_cols <- c("match_id", "competition_id", "home_team", "away_team", "season_id", "match_date")
  
  check_arg_results(results, expected_cols)
  
  if (is.na(rows[1])) {
    
    rows <- 1:nrow(results)
    
  } else {
    
    # Add some checks of rows argument
    
  }
  
  results_to_add_matches <- results %>% 
    slice(rows) %>% 
    select(competition_id, season_id) %>% 
    distinct() %>% 
    semi_join(results, ., by = c("competition_id", "season_id"))
 
  if (nrow(results_to_add_matches) > 0) {
    
    existing_results <- filter(results, !(match_id %in% results_to_add_matches$match_id))
    
    results_subset_with_matches_played <- results_to_add_matches %>%
      pivot_longer(cols = home_team:away_team, names_to = "location", values_to = "team") %>%
      group_by(season_id, team) %>%
      mutate(matches_played_season = 1:n()) %>%
      ungroup()
    
    results_subset_with_matches_played_home <- filter(results_subset_with_matches_played, location == "home_team")
    results_subset_with_matches_played_away <- filter(results_subset_with_matches_played, location == "away_team") 
    
    results_with_matches_played_season <- results_to_add_matches %>%
      mutate(home_matches_played_season = results_subset_with_matches_played_home$matches_played_season,
             away_matches_played_season = results_subset_with_matches_played_away$matches_played_season)
    
    results_updated <- bind_rows(existing_results, results_to_add_matches)
    
    return(results_updated)
    
  } else {
    
    return(results)
    
  }

}
