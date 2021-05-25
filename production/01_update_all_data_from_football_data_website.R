
# TITLE: 
#   Updating Football Data Databases
#
# DESCRIPTION:
#   Updating  www.football-data.co.uk Results and Odds Databases

# Download new data ----------------------------------------------------------------------------------------------------

wd <- load_wd()

## Clear previous downloads

files_previous_download <- list.files(wd$live_data_football_data_co_uk_latest_csvs, full.names = TRUE)

if(length(files_previous_download) > 0){
  
  invisible(file.remove(files_previous_download))
  
}

## Download

metadata <- load_metadata()
 
competition_ids_main <- metadata$main_leagues$competition_id
competition_ids_extra <- metadata$extra_leagues$competition_id

season_ids_main <- metadata$main_leagues$end_season

list(competition_id = competition_ids_main, season_ids = season_ids_main) %>%
  pwalk(get_from_football_data, path = wd$live_data_football_data_co_uk_latest_csvs)

walk(competition_ids_extra, get_from_football_data, path = wd$live_data_football_data_co_uk_latest_csvs)

# Clean latest downloads -----------------------------------------------------------------------------------------------

# Latest files

files <- list.files(wd$live_data_football_data_co_uk_latest_csvs, full.names = TRUE)
files_main <- files %>% str_detect(pattern = "_all", negate = TRUE) %>% magrittr::extract(files, .)
files_extra <- files %>% str_detect(pattern = "_all") %>% magrittr::extract(files, .)

# New Results

results_new_main <- map_dfr(files_main, create_db_table_results_main)
results_new_extra <- map_dfr(files_extra, create_db_table_results_main, data_type = "fd_extra")
results_new <- bind_rows(results_new_main,results_new_extra)

# Compare with existing data for duplicates

database_results_previous <- wd$live_data %>%paste0("football_results.rds") %>% read_rds() 

# delete
# database_results_previous <- database_results_previous %>% filter(match_date < max(match_date) - 2)

# At this point we can add in the independent features to the new data

results_new_unique <- results_new %>% 
  filter(!(match_id %in% database_results_previous$match_id)) %>%
  add_empty_stadiums()

num_matches_new <- nrow(results_new_unique)

if(num_matches_new > 0){
  
  num_matches_previous <- nrow(database_results_previous)
  rows_to_add_features <- (num_matches_previous + 1):(num_matches_previous + num_matches_new)
  
  # Here we add in the dependent features i.e. features which depend on previous entries, but only for the new rows for
  # performance reasons. When adding in a new feature need to go back to the development folder and re-run the build for
  # the whole database to create the new db with all the correct features.
  
  database_results <- database_results_previous %>% 
    bind_rows(results_new_unique) %>%
    add_matches_played(rows = rows_to_add_features)
  
  wd$live_data %>% paste0("football_results.rds") %>% write_rds(database_results, .)
  
  # Copy over the latest files to raw directory
  
  files_short <- list.files(wd$live_data_football_data_co_uk_latest_csvs)
  files_to <- paste0(wd$live_data_football_data_co_uk_historic_csvs, files_short)
  
  invisible(file.copy(files, files_to, overwrite = TRUE))
}
