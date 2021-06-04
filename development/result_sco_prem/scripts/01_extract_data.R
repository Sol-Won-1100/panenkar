
# TITLE:       Extract Data
# DESCRIPTION: Extract data for testing from live database

# Setup ----------------------------------------------------------------------------------------------------------------

# Parameters

.competition_id <- "sco_prem"
wd <- load_wd()

# DELTE

# Set paths
# wd$wd <- glue("{here::here()}/")
# wd$live_data <- glue("{wd$wd}live_data/")
# wd$subproject <- glue("{wd$wd}{subproject_path}")
# wd$raw_data <- glue("{wd$subproject}raw_data/")

# Query results and save as rds file -----------------------------------------------------------------------------------

# Drop the database setup - just use csv file in future but already outputted the rds

season_ids <- sequence_seasons("1994_1995", "2018_2019")

glue("{wd$live_data}football_results.rds") %>%
  read_rds() %>%
  filter(competition_id == .competition_id, season_id %in% season_ids) %>%
  write_rds(glue("{wd$dev_result_sco_prem_raw_data}results_main_{.competition_id}.rds"))
  