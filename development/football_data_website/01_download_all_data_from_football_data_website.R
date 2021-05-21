
# NAME:        Initial Download from Football Data Website
# DESCRIPTION: Downloading  www.football-data.co.uk football results and odds data. Saves csvs as is into 
#              football-production/live-data/football_data_co_uk_historic_csvs/ for further manipulation

wd <- load_wd()
metadata <- load_metadata()

competition_ids_main <- metadata$main_leagues$competition_id
competition_ids_extra <- metadata$extra_leagues$competition_id
season_ids_main_start <- metadata$main_leagues$start_season
season_ids_main_end <- metadata$main_leagues$end_season

season_ids_main <- map2(season_ids_main_start, season_ids_main_end, sequence_seasons)

# Download

list(competition_id = competition_ids_main, season_ids = season_ids_main) %>%
  pwalk(get_from_football_data, path = wd$live_data_football_data_co_uk_historic_csvs)

walk(competition_ids_extra, get_from_football_data, path = wd$live_data_football_data_co_uk_historic_csvs)
