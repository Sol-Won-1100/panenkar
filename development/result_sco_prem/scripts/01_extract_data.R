
# TITLE:       Extract Data
# DESCRIPTION: Extract data for testing from live database

# Setup ----------------------------------------------------------------------------------------------------------------

# Libraries

library(panenkar)

# Parameters

.competition_id <- "sco_prem"
subproject_path <- "development/results_sco_prem/"

# Set paths

wd <- list()

wd$wd <- glue("{here::here()}/")
wd$live_data <- glue("{wd$wd}live_data/")
wd$subproject <- glue("{wd$wd}{subproject_path}")
wd$raw_data <- glue("{wd$subproject}raw_data/")

# Query database and save as rds file ----------------------------------------------------------------------------------

season_ids <- sequence_seasons("1994_1995", "2018_2019")

con <- connect_to_access_dbi(paste0(wd$live_data, "football_database.accdb"))

results <- con %>%
  tbl("results_main") %>%
  filter(competition_id == .competition_id, season_id %in% season_ids) %>%
  as_tibble()
  
write_rds(results, paste0(wd$raw_data, "results_main_", .competition_id, ".rds"))
  