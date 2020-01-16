
#' @title Clean odds from football data
#'
#' @description Clean results data from www.football-data.co.uk main leagues and
#' extra leagues
#'
#' @param file name of file downloaded from football-data.co.uk
#' @param data_type one of "main_league", "extra_league". See get_metdata.
#'
#' @export
#'
#' @examples
#' file <- "C:/Users/Neil/Documents/football-betting/data/raw/eng_pl_2018_2019.csv"
#' clean_odds_fd(file)

clean_odds_fd <- function(file, data_type = "main_league"){
  
  if(data_type == "main_league"){
    odds <- clean_odds_fdm(file)
  } else if(data_type == "extra_league"){
    odds <- clean_odds_fde(file)
  } else {
    stop("bad data_type supplied")
  }
  
  return(manual_clean_teams(odds))
}


#' @title Clean odds from football data main leagues
#'
#' @description Clean results data from www.football-data.co.uk main leagues
#'
#' @param file name of file downloaded from football-data.co.uk
#'
#' @export
#'
#' @examples
#' file <- "C:/Users/Neil/Documents/football-betting/data/raw/eng_pl_2018_2019.csv"
#' clean_odds_fdm(file)

clean_odds_fdm <- function(file){
  
  # Some initial prep work getting competition_id, season_id, etc into the
  # correct format.
  
  file_short <- str_split(file, pattern = "/") %>%
    unlist() %>% rev() %>%
    extract(1) %>%
    str_replace(pattern = ".csv", replacement = "")
  
  season_id <- file_short %>% str_extract(pattern = "[0-9]{4}_[0-9]{4}")
  
  competition_id <- str_replace(file_short, pattern = season_id,
                                replacement = "") %>%
    nchar() %>%
    subtract(1) %>%
    str_sub(file_short, 1, . )
  
  # On some of the greek files HT and AT are in there instead of HomeTeam and
  # AwayTeam
  
  col_names_dirty <- c("Date", "HomeTeam", "AwayTeam", "HT", "AT")
  col_names_clean <- c("match_date", rep(c("home_team", "away_team"), 2))
  
  # The lookup table will contain all possible bookmaker names from the football
  # data datasets
  
  bookmaker_lookup <- get_bookmaker_lookup_football_data()
  col_names_odds <- bookmaker_lookup %>% select(football_data_code) %>% unlist()
  
  # Read in the dataset
  suppressWarnings(
    suppressMessages(
      fd_raw <- read_csv_robust(file)
    )
  )
  
  
  if(fd_raw$error == TRUE){
    stop(fd_raw$error_message)
  } else {
    fd_raw <- fd_raw$csv
  }
  
  names(fd_raw)[colnames(fd_raw) %in% col_names_dirty] <- unique(col_names_clean)
  
  fd_clean_names <- filter(fd_raw, !is.na(match_date))
  
  # If some of the expected match data columns are missing fill them in with
  # NAs and bind to the dataset
  
  col_names_missing <- subset(
    col_names_odds,
    !(col_names_odds %in% colnames(fd_clean_names))
  )
  
  if(length(col_names_missing) > 0){
    
    fd_clean_names <- NA %>%
      matrix(nrow = nrow(fd_clean_names), ncol = length(col_names_missing)) %>%
      set_colnames(col_names_missing) %>%
      as_tibble() %>%
      bind_cols(fd_clean_names, .)
    
  }
  
  col_names_select <- unique(c("match_id", "competition_id", "season_id",
                               "match_date", "match_time", col_names_clean,
                               col_names_odds))
  
  # Final tidying
  fd <- fd_clean_names %>%
    mutate(match_date = dmy(match_date),
           match_time = NA,
           season_id = season_id,
           competition_id = competition_id,
           match_id = paste0(competition_id, ".",
                             match_date, ".",
                             home_team, ".",
                             away_team)) %>%
    select(col_names_select) %>%
    gather(key = "football_data_code", value = "odds", B365H:`Avg<2.5`) %>%
    arrange(desc(match_id)) %>% # remove this line - not needed
    filter(!is.na(odds)) %>%
    left_join(bookmaker_lookup, by = "football_data_code") %>%
    select(match_id:away_team, measurement:selection, odds)
  
  
  return(fd)
  
}

#' @title Clean odds from football data extra leagues
#'
#' @description Clean results data from www.football-data.co.uk extra leagues
#'
#' @param file name of file downloaded from football-data.co.uk
#'
#' @export
#'
#' @examples
#' file <- "C:/Users/Neil/Documents/football-betting/data/raw/eng_pl_2018_2019.csv"
#' clean_odds_fde(file)

clean_odds_fde <- function(file){
  
  file_short <- str_split(file, pattern = "/") %>%
    unlist() %>% rev() %>%
    extract(1) %>%
    str_replace(pattern = ".csv", replacement = "")
  
  competition_id <- str_replace(file_short, pattern = "_all", replacement = "")
  
  col_names_dirty <- c("Date", "Time", "Home", "Away")
  col_names_clean <- c("match_date", "match_time", "home_team", "away_team")
  
  # The lookup table will contain all possible bookmaker names from the football
  # data datasets
  
  bookmaker_lookup <- get_bookmaker_lookup_football_data()
  col_names_odds <- bookmaker_lookup %>% select(football_data_code) %>% unlist()
  
  # Read in the dataset
  suppressWarnings(
    suppressMessages(
      fd_raw <- read_csv_robust(file)
    )
  )
  
  if(fd_raw$error == TRUE){
    stop(fd_raw$error_message)
  } else {
    fd_raw <- fd_raw$csv
  }
  
  names(fd_raw)[colnames(fd_raw) %in% col_names_dirty] <- col_names_clean
  
  fd_clean_names <- filter(fd_raw, !is.na(match_date))
  
  # If some of the expected match data columns are missing fill them in with
  # NAs and bind to the dataset
  
  col_names_missing <- subset(
    col_names_odds,
    !(col_names_odds %in% colnames(fd_clean_names))
  )
  
  if(length(col_names_missing) > 0){
    
    fd_clean_names <- NA %>%
      matrix(nrow = nrow(fd_clean_names), ncol = length(col_names_missing)) %>%
      set_colnames(col_names_missing) %>%
      as_tibble() %>%
      bind_cols(fd_clean_names, .)
    
  }
  
  col_names_select <- unique(c("match_id", "competition_id", "season_id",
                               "match_date", "match_time", col_names_clean,
                               col_names_odds))
  
  # Final tidying
  fd <- fd_clean_names %>%
    mutate(match_date = dmy(match_date),
           season_id = str_replace(Season, "/", "_"),
           competition_id = competition_id,
           match_id = paste0(competition_id, ".",
                             match_date, ".",
                             home_team, ".",
                             away_team)) %>%
    select(col_names_select) %>%
    gather(key = "football_data_code", value = "odds", B365H:`Avg<2.5`) %>%
    arrange(desc(match_id)) %>% # remove this line - not needed
    filter(!is.na(odds)) %>%
    left_join(bookmaker_lookup, by = "football_data_code") %>%
    select(match_id:away_team, measurement:selection, odds)
  
  
  return(fd)
}

#' @title Get bookmaker lookup table for football data bookmakers
#'
#' @description A quick way to retrieve a lookup table which translates the
#' names of the bookmakers in the www.football-data.co.uk datasets into a
#' cleaner format
#' @export

get_bookmaker_lookup_football_data <- function(){
  lookup_bookmakers_football_data <- tribble(
    ~football_data_code, ~measurement,   ~bookmaker_name, ~aggregate_type, ~commission,          ~market, ~selection,
    "B365H",   "standard",         "bet_365",              NA,           0,      "full_time",     "home",
    "B365D",   "standard",         "bet_365",              NA,           0,      "full_time",     "draw",
    "B365A",   "standard",         "bet_365",              NA,           0,      "full_time",     "away",
    "BSH",   "standard",     "blue_square",              NA,           0,      "full_time",     "home",
    "BSD",   "standard",     "blue_square",              NA,           0,      "full_time",     "draw",
    "BSA",   "standard",     "blue_square",              NA,           0,      "full_time",     "away",
    "BWH",   "standard",     "bet_and_win",              NA,           0,      "full_time",     "home",
    "BWD",   "standard",     "bet_and_win",              NA,           0,      "full_time",     "draw",
    "BWA",   "standard",     "bet_and_win",              NA,           0,      "full_time",     "away",
    "GBH",   "standard",      "gamebooker",              NA,           0,      "full_time",     "home",
    "GBD",   "standard",      "gamebooker",              NA,           0,      "full_time",     "draw",
    "GBA",   "standard",      "gamebooker",              NA,           0,      "full_time",     "away",
    "IWH",   "standard",     "interwetten",              NA,           0,      "full_time",     "home",
    "IWD",   "standard",     "interwetten",              NA,           0,      "full_time",     "draw",
    "IWA",   "standard",     "interwetten",              NA,           0,      "full_time",     "away",
    "LBH",   "standard",       "ladbrokes",              NA,           0,      "full_time",     "home",
    "LBD",   "standard",       "ladbrokes",              NA,           0,      "full_time",     "draw",
    "LBA",   "standard",       "ladbrokes",              NA,           0,      "full_time",     "away",
    "PSH",   "standard", "pinnacle_sports",              NA,           0,      "full_time",     "home",
    "PSD",   "standard", "pinnacle_sports",              NA,           0,      "full_time",     "draw",
    "PSA",   "standard", "pinnacle_sports",              NA,           0,      "full_time",     "away",
    "PH",   "standard", "pinnacle_sports",              NA,           0,      "full_time",     "home",
    "PD",   "standard", "pinnacle_sports",              NA,           0,      "full_time",     "draw",
    "PA",   "standard", "pinnacle_sports",              NA,           0,      "full_time",     "away",
    "SOH",   "standard",   "sporting_odds",              NA,           0,      "full_time",     "home",
    "SOD",   "standard",   "sporting_odds",              NA,           0,      "full_time",     "draw",
    "SOA",   "standard",   "sporting_odds",              NA,           0,      "full_time",     "away",
    "SBH",   "standard",    "sporting_bet",              NA,           0,      "full_time",     "home",
    "SBD",   "standard",    "sporting_bet",              NA,           0,      "full_time",     "draw",
    "SBA",   "standard",    "sporting_bet",              NA,           0,      "full_time",     "away",
    "SJH",   "standard",      "stan_james",              NA,           0,      "full_time",     "home",
    "SJD",   "standard",      "stan_james",              NA,           0,      "full_time",     "draw",
    "SJA",   "standard",      "stan_james",              NA,           0,      "full_time",     "away",
    "SYH",   "standard",     "stanley_bet",              NA,           0,      "full_time",     "home",
    "SYD",   "standard",     "stanley_bet",              NA,           0,      "full_time",     "draw",
    "SYA",   "standard",     "stanley_bet",              NA,           0,      "full_time",     "away",
    "VCH",   "standard",          "vc_bet",              NA,           0,      "full_time",     "home",
    "VCD",   "standard",          "vc_bet",              NA,           0,      "full_time",     "draw",
    "VCA",   "standard",          "vc_bet",              NA,           0,      "full_time",     "away",
    "WHH",   "standard",    "william_hill",              NA,           0,      "full_time",     "home",
    "WHD",   "standard",    "william_hill",              NA,           0,      "full_time",     "draw",
    "WHA",   "standard",    "william_hill",              NA,           0,      "full_time",     "away",
    "BbMxH",  "aggregate",       "bet_brain",       "maximum",           0,      "full_time",     "home",
    "BbAvH",  "aggregate",       "bet_brain",       "average",           0,      "full_time",     "home",
    "BbMxD",  "aggregate",       "bet_brain",       "maximum",           0,      "full_time",     "draw",
    "BbAvD",  "aggregate",       "bet_brain",       "average",           0,      "full_time",     "draw",
    "BbMxA",  "aggregate",       "bet_brain",       "maximum",           0,      "full_time",     "away",
    "BbAvA",  "aggregate",       "bet_brain",       "average",           0,      "full_time",     "away",
    "MaxH",  "aggregate",     "odds_portal",       "maximum",           0,      "full_time",     "home",
    "MaxD",  "aggregate",     "odds_portal",       "maximum",           0,      "full_time",     "draw",
    "MaxA",  "aggregate",     "odds_portal",       "maximum",           0,      "full_time",     "away",
    "AvgH",  "aggregate",     "odds_portal",       "average",           0,      "full_time",     "home",
    "AvgD",  "aggregate",     "odds_portal",       "average",           0,      "full_time",     "draw",
    "AvgA",  "aggregate",     "odds_portal",       "average",           0,      "full_time",     "away",
    "BbMx>2.5",  "aggregate",       "bet_brain",       "maximum",           0, "over_under_2.5",     "over",
    "BbAv>2.5",  "aggregate",       "bet_brain",       "average",           0, "over_under_2.5",     "over",
    "BbMx<2.5",  "aggregate",       "bet_brain",       "maximum",           0, "over_under_2.5",    "under",
    "BbAv<2.5",  "aggregate",       "bet_brain",       "average",           0, "over_under_2.5",    "under",
    "GB>2.5",   "standard",      "gamebooker",              NA,           0, "over_under_2.5",     "over",
    "GB<2.5",   "standard",      "gamebooker",              NA,           0, "over_under_2.5",    "under",
    "B365>2.5",   "standard",      "gamebooker",              NA,           0, "over_under_2.5",     "over",
    "B365<2.5",   "standard",      "gamebooker",              NA,           0, "over_under_2.5",    "under",
    "P>2.5",   "standard", "pinnacle_sports",              NA,           0, "over_under_2.5",     "over",
    "P<2.5",   "standard", "pinnacle_sports",              NA,           0, "over_under_2.5",    "under",
    "Max>2.5",  "aggregate",     "odds_portal",       "maximum",           0, "over_under_2.5",     "over",
    "Max<2.5",  "aggregate",     "odds_portal",       "maximum",           0, "over_under_2.5",    "under",
    "Avg>2.5",  "aggregate",     "odds_portal",       "average",           0, "over_under_2.5",     "over",
    "Avg<2.5",  "aggregate",     "odds_portal",       "average",           0, "over_under_2.5",    "under"
  )
}


