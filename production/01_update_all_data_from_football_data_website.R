
# TITLE: 
#   Updating Football Data Databases
#
# DESCRIPTION:
#   Updating  www.football-data.co.uk Results and Odds Databases

# Setup ----------------------------------------------------------------------------------------------------------------

wd <- load_wd()

# Clear the old data from the folder

files_old <- list.files(wd$data_raw_latest, full.names = TRUE)

if(length(files_old) > 0){
  invisible(
    file.remove(files_old)
  )
  
}

metadata <- get_metadata(latest_european_season = "2020_2021")

league_types <- metadata %>%
  map("football_data") %>%
  map("data_type") %>%
  unlist()

competition_ids_main <- league_types %>% 
  subset(league_types == "main_league") %>% 
  names()

competition_ids_extra <- league_types %>% 
  subset(league_types == "extra_league") %>% 
  names()

season_ids_main <- metadata[competition_ids_main] %>%
  map("football_data") %>%
  map("end_season") 

# Download

list(competition_id = competition_ids_main, season_ids = season_ids_main) %>%
  pwalk(get_from_football_data, path = wd$data_raw_latest)

walk(competition_ids_extra, get_from_football_data, path = wd$data_raw_latest)

# Clean latest downloads -------------------------------------------------------

# Latest files

files <- list.files(wd$data_raw_latest, full.names = TRUE)

files_main <- files %>% 
  str_detect(pattern = "_all", negate = TRUE) %>% 
  magrittr::extract(files, .)

files_extra <- files %>% 
  str_detect(pattern = "_all") %>% 
  magrittr::extract(files, .)

# New Results

results_new_main <- map_dfr(files_main, clean_results_fd)

results_new_extra <- map_dfr(files_extra, clean_results_fd, data_type = "extra_league")

results_new <- bind_rows(results_new_main,results_new_extra)

# Compare with existing data for duplicates
database_results_previous <- wd$data_clean %>%
  paste0("database-results.rds") %>%
  read_rds() 

results_new_unique <- filter(results_new, 
                             !(match_id %in% database_results_previous$match_id))

num_matches_new <- nrow(results_new_unique)

if(num_matches_new > 0){
  
  num_matches_previous <- nrow(database_results_previous)
  rows <- (num_matches_previous + 1):(num_matches_previous + num_matches_new)
  
  database_results <- database_results_previous %>%
    bind_rows(results_new_unique) %>%
    add_dependent_features_fd()
  
  wd$data_clean %>%
    paste0("database-results.rds") %>%
    write_rds(database_results_football_data, .)
  
  # Copy over the latest files to raw directory
  
  files_short <- list.files(wd$data_raw_latest)
  files_to <- paste0(wd$data_raw_latest, files_short)
  
  invisible(
    file.copy(files, files_to)
  )
}







