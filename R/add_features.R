
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
    filter(!is.na(match_ids_empty_stadium))
    select(match_ids_empty_stadium) %>%
    unlist() %>%
    str_split(";") %>%
    unlist() %>%
    str_replace_all('\"', "") %>%
    str_replace_all("\'", "")

  match_ids_extracted_partial_empty_stadium <- metadata_competitions %>%
      filter(!is.na(match_ids_partial_empty_stadium))
      select(match_ids_partial_empty_stadium) %>%
      unlist() %>%
      str_split(";") %>%
      unlist() %>%
      str_replace_all('\"', "") %>%
      str_replace_all("\'", "")
  
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
