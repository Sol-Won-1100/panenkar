
# TITLE:       Extract Data
# DESCRIPTION: Extract data for testing from live database

# Setup ----------------------------------------------------------------------------------------------------------------

# Libraries

library(panenkar)
library(tidyverse)
library(DBI)

# Update

competition <- "sco_prem"

# Set paths

wd <- list()

wd$wd <- here::here() %>% paste0("/")
wd$live_data <- paste0(wd$wd, "live-data/")
wd$project_data <- paste0(wd$wd, "development/full-time-result/", competition, "/data/")

# Query database and save as rds file ----------------------------------------------------------------------------------

season_ids <- sequence_seasons("1994_1995", "2018_2019")

con <- connect_to_access_dbi(paste0(wd$live_data, "football-database.accdb"))

results <- con %>%
  tbl("results_main") %>%
  filter(competition_id == competition, season_id %in% season_ids) %>%
  as_tibble()
  
write_rds(results, paste0(wd$project_data, "results-main-", competition, ".rds"))
  