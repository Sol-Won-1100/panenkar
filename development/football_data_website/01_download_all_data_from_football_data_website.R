
# NAME:        Initial Download from Football Data Website
# DESCRIPTION: Downloading  www.football-data.co.uk football results and odds data. Saves csvs as is into 
#              football-production/live-data/football-data-co-uk_historic-csvs/ for further manipulation

# Setup ----------------------------------------------------------------------------------------------------------------

# Libraries

library(panenkar)
library(here)
library(magrittr)
library(dplyr)
library(readr)
library(purrr)
library(stringr)

# Working directories

wd <- list()

wd$wd <- here::here() %>% paste0("/")
wd$live_data <- paste0(wd$wd, "live-data/")
wd$metadata <- paste0(wd$live_data, "metadata/")
wd$football_data_co_uk_historic_csvs <- paste0(wd$live_data, "football-data-co-uk_historic-csvs/")

# Get data -------------------------------------------------------------------------------------------------------------

# Metadata

metadata <- list()

metadata$competitions <- wd$metadata %>% 
  paste0("competitions.csv") %>% 
  read_csv(col_types = cols(.default = col_character(), tier = col_double()))

metadata$main_leagues <- wd$metadata %>%
  paste0("football-data-co-uk_main-leagues.csv") %>%
  read_csv(col_types = cols(.default = col_character()))

metadata$extra_leagues <- wd$metadata %>%
  paste0("football-data-co-uk_extra-leagues.csv") %>%
  read_csv(col_types = cols(.default = col_character()))

competition_ids_main <- metadata$main_leagues$competition_id
competition_ids_extra <- metadata$extra_leagues$competition_id
season_ids_main_start <- metadata$main_leagues$start_season
season_ids_main_end <- metadata$main_leagues$end_season

season_ids_main <- map2(season_ids_main_start, season_ids_main_end, sequence_seasons)

# Download

list(competition_id = competition_ids_main, season_ids = season_ids_main) %>%
  pwalk(get_from_football_data, path = wd$football_data_co_uk_historic_csvs)

walk(competition_ids_extra, get_from_football_data, path = wd$football_data_co_uk_historic_csvs)


