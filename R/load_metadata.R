
#' Load Metadata
#' 
#' Load in metadata files
#' 
#' @return metadata list
#' @export

load_metadata <- function() {
  
  metadata <- list()
  
  metadata$competitions <- wd$metadata %>% 
    paste0("competitions.csv") %>% 
    read_csv(col_types = cols(.default = col_character(), 
                              tier = col_double())) %>%
    mutate(start_date_covid_empty_stadium = dmy(start_date_covid_empty_stadium),
           end_date_covid_empty_stadium = dmy(end_date_covid_empty_stadium),
           start_date_covid_partial_empty_stadium = dmy(start_date_covid_partial_empty_stadium),
           end_date_covid_partial_empty_stadium = dmy(end_date_covid_partial_empty_stadium))

  metadata$main_leagues <- wd$metadata %>%
    paste0("football_data_co_uk_main_leagues.csv") %>%
    read_csv(col_types = cols(.default = col_character()))
  
  metadata$extra_leagues <- wd$metadata %>%
    paste0("football_data_co_uk_extra_leagues.csv") %>%
    read_csv(col_types = cols(.default = col_character()))
  
  return(metadata)
  
}