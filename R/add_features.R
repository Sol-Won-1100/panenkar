
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

add_empty_stadiums <- function(results) {
  
  if (check_arg_results(results) == FALSE ) {
    
    stop("'results' must be a results tibble")
    
  }
  
  load_metadata()$competitions %>%
    select(competition_id, start_date_covid_empty_stadium, end_date_covid_empty_stadium, 
           start_date_covid_partial_empty_stadium, end_date_covid_partial_empty_stadium) %>%
    left_join(results_both_leagues, ., by = "competition_id") %>%
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
        TRUE ~ FALSE
      ),
      
      is_partial_empty_stadium = case_when(
        match_date >= start_date_covid_partial_empty_stadium & match_date <= end_date_covid_partial_empty_stadium ~ TRUE,
        TRUE ~ FALSE
      )
    )
}
