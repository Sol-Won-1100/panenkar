
# TITLE:       Clean All Football Data from Football Data Website
# DESCRIPTION: Cleans all the csvs in football-production/live-data/football-data-co-uk_historic-csvs/ and adds the data
#              as tables to football-production/live-data/football-database.accdb

# Setup ----------------------------------------------------------------------------------------------------------------

wd <- list()

wd$wd <- here::here() %>% paste0("/")
wd$live_data <- paste0(wd$wd, "live_data/")
wd$metadata <- paste0(wd$live_data, "metadata/")
wd$football_data_co_uk_historic_csvs <- paste0(wd$live_data, "football_data_co_uk_historic_csvs/")

# Clean and output file for database -----------------------------------------------------------------------------------

## Raw files

files <- list.files(wd$football_data_co_uk_historic_csvs, full.names = TRUE)

files_main <- files %>% 
  str_detect(pattern = "_all", negate = TRUE) %>% 
  magrittr::extract(files, .)

files_extra <- files %>% 
  str_detect(pattern = "_all") %>% 
  magrittr::extract(files, .)

results_main_leagues <- map_dfr(files_main, create_db_table_results_main)
results_extra_leagues <- map_dfr(files_extra, create_db_table_results_main,  data_type = "fd_extra")


# Add time independent features to dataset -----------------------------------------------------------------------------

results_both_leagues <- results_main_leagues %>% 
  bind_rows(results_extra_leagues) %>%
  add_empty_stadiums()

## Push to results main

# It is quicker to write the dataset locally then push locally to the database than direct

filename_database <- paste0(wd$live_data, "football_results.csv")

write_csv(results_both_leagues, filename_database, col_names = FALSE)
