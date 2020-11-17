
# TITLE:       Clean All Football Data from Football Data Website
# DESCRIPTION: Cleans all the csvs in football-production/live-data/football-data-co-uk_historic-csvs/ and adds the data
#              as tables to football-production/live-data/football-database.accdb

# Setup ----------------------------------------------------------------------------------------------------------------

# Libraries

library(panenkar)
library(lubridate)
library(magrittr) 
library(tidyverse)
library(DBI)

# Set paths

wd <- list()

wd$wd <- here::here() %>% paste0("/")
wd$live_data <- paste0(wd$wd, "live-data/")
wd$metadata <- paste0(wd$live_data, "metadata/")
wd$football_data_co_uk_historic_csvs <- paste0(wd$live_data, "football-data-co-uk_historic-csvs/")

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
results_both_leagues <-  bind_rows(results_main_leagues, results_extra_leagues)

## Push to results main

# It is quicker to write the dataset locally then push locally to the database than direct

file_db_setup <- paste0(wd$live_data, "football-database-setup.csv")

write_csv(results_both_leagues, file_db_setup, col_names = FALSE)
