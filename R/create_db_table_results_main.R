
#' Create Database Table Results Main
#' 
#' Creates the data frame in the format required by the results main table in the live football database
#' 
#' @param file filename of the raw football match data from football-data.co.uk
#' @param data_type one of fd_main or fd_extra depending on league type
#' 
#' @return results main tibble ready for pushing to database
#' 
#' @export

create_db_table_results_main <- function (file, data_type = "fd_main") {
  
  if (data_type == "fd_main") {
    
    results_main <- create_db_table_results_main_fd_main(file)
    
  } else if (data_type == "fd_extra") {
    
    results_main <- create_db_table_results_main_fd_extra(file)
    
  } else {
    
    stop ("bad data_type supplied")
    
  }
  
  return (results_main)
  
}


#' Create Database Table Results Main Football Data Main Leagues
#' 
#' Creates the data frame in the format required by the results main table in the live football database for main 
#' leagues from football-data.co.uk
#' 
#' @param file filename of the raw football match data from football-data.co.uk
#' 
#' @return results main tibble ready for pushing to database

create_db_table_results_main_fd_main <- function (file) {
  
  if (str_ends(file, ".csv") == FALSE & str_ends(file, ".txt") == FALSE) {
    
    stop ("expected .csv or .txt in filen")
    
  }
  
  file_short <- str_split(file, pattern = "/") %>%
    unlist() %>% 
    rev() %>%
    magrittr::extract(1) %>%
    str_replace(pattern = ".csv", replacement = "")
  
  season_id <- file_short %>% str_extract(pattern = "[0-9]{4}_[0-9]{4}")
  
  if (is.na(season_id)) {
    
    stop ("bad season_id in file, expecting id of the form XXXX_XXXX in file name")
    
  }
  
  competition_id <- str_replace(file_short, pattern = season_id,
                                replacement = "") %>%
    nchar() %>%
    map(1) %>%
    unlist() %>%
    str_sub(file_short, 1, . )
  
  
  col_names_lookup <- tribble(
    ~dirty,       ~clean,
    # ***********/ ***********
    
    "Date",       "match_date",
    "HomeTeam",   "home_team",
    "AwayTeam",   "away_team",
    "FTHG",       "home_goals",
    "FTAG",       "away_goals",
    "GBH",        "home_odds_gamebooker",
    "GBD",        "draw_odds_gamebooker",
    "GBA",        "away_odds_gamebooker",
    "IWH",        "home_odds_interwetten",
    "IWD",        "draw_odds_interwetten",
    "IWA",        "away_odds_interwetten",
    "SBH",        "home_odds_sportingbet",
    "SBD",        "draw_odds_sportingbet",
    "SBA",        "away_odds_sportingbet",
    "WHH",        "home_odds_williamhill",     
    "WHD",        "draw_odds_williamhill",     
    "WHA",        "away_odds_williamhill",      
    "SYH",        "home_odds_stanleybet",     
    "SYD",        "draw_odds_stanleybet",      
    "SYA",        "away_odds_stanleybet",   
    "B365H",      "home_odds_bet365",
    "B365D",      "draw_odds_bet365",
    "B365A",      "away_odds_bet365",
    "GB>2.5",     "over_odds_gamebooker",
    "GB<2.5",     "under_odds_gamebooker",
    "BWH",        "home_odds_betandwin",      
    "BWD",        "draw_odds_betandwin",        
    "BWA",        "away_odds_betandwin",        
    "LBH",        "home_odds_ladbrokes",        
    "LBD",        "draw_odds_ladbrokes",
    "LBA",        "away_odds_ladbrokes",  
    "SJH",        "home_odds_stanjames",
    "SJD",        "draw_odds_stanjames",
    "SJA",        "away_odds_stanjames",
    "VCH",        "home_odds_vcbet",
    "VCD",        "draw_odds_vcbet",
    "VCA",        "away_odds_vcbet",
    "BbMxH",      "max_home_odds_betbrain",
    "BbMxD",      "max_draw_odds_betbrain",
    "BbMxA",      "max_away_odds_betbrain",
    "BbMx>2.5",   "max_over_odds_betbrain",
    "BbMx<2.5",   "max_under_odds_betbrain",
    "BSH",        "home_odds_bluesquare",
    "BSD",        "draw_odds_bluesquare",
    "BSA",        "away_odds_bluesquare",
    "PSCH",       "closing_home_odds_pinnacle",   
    "PSCD",       "closing_draw_odds_pinnacle",  
    "PSCA",       "closing_away_odds_pinnacle",  
    "B365>2.5",   "over_odds_bet365",
    "B365<2.5",   "under_odds_bet365",
    "PC>2.5",     "closing_over_odds_pinnacle",
    "PC<2.5",     "closing_under_odds_pinnacle",
    "SOH",        "home_odds_sportingodds",
    "SOD",        "draw_odds_sportingodds",
    "SOA",        "away_odds_sportingodds",
    "MaxH",       "max_home_odds_market",
    "MaxD",       "max_draw_odds_market",
    "MaxA",       "max_away_odds_market",
    "Max>2.5",    "max_over_odds_market",
    "Max<2.5",    "max_under_odds_market"
  )
  
  # Read in the dataset

  suppressWarnings(
    suppressMessages(
      fd_raw <- read_csv_robust(file)
    )
  )
  
  if (fd_raw$error == TRUE) {
    
    stop(fd_raw$error_message)
    
  } else {
    
    fd_raw <- fd_raw$csv
    
  }
  
  col_names_in_raw <- subset(col_names_lookup$dirty, col_names_lookup$dirty %in% colnames(fd_raw))
  
  fd_clean_names <- fd_raw %>%
    select(all_of(col_names_in_raw)) %>%
    set_colnames(subset(col_names_lookup$clean,
                        col_names_lookup$dirty %in% col_names_in_raw)) %>%
    filter(!is.na(match_date))
  
  col_names_not_in_raw <- subset(col_names_lookup$clean,
                                 !(col_names_lookup$dirty %in% colnames(fd_raw)))
  
  if (length(col_names_not_in_raw) > 0) {
    
    fd_clean_names <- NA %>%
      matrix(nrow = nrow(fd_clean_names), ncol = length(col_names_not_in_raw)) %>%
      set_colnames(col_names_not_in_raw) %>%
      as_tibble() %>%
      bind_cols(fd_clean_names, .)
    
  }

  results_main <- fd_clean_names %>%
    mutate(match_date = dmy(match_date),
           season_id = season_id,
           competition_id = competition_id,
           home_odds_max_data = calc_max_odds(fd_clean_names, "home_odds"),
           draw_odds_max_data = calc_max_odds(fd_clean_names, "draw_odds"),
           away_odds_max_data = calc_max_odds(fd_clean_names, "away_odds"),
           over_odds_max_data = calc_max_odds(fd_clean_names, "over_odds"),
           under_odds_max_data = calc_max_odds(fd_clean_names, "under_odds"),
           result = case_when(home_goals > away_goals ~ "home",
                              home_goals == away_goals ~ "draw",
                              home_goals < away_goals ~ "away",
                              TRUE ~ NA_character_),
           total_goals = home_goals + away_goals,
           over_under_2_5 = case_when(
             total_goals < 2.5 ~ "under",
             total_goals > 2.5 ~ "over",
             TRUE ~ NA_character_
           ),
           home_odds_sharp_closing = as.numeric(closing_home_odds_pinnacle),
           draw_odds_sharp_closing = as.numeric(closing_draw_odds_pinnacle),
           away_odds_sharp_closing = as.numeric(closing_away_odds_pinnacle),
           over_odds_sharp_closing = as.numeric(closing_over_odds_pinnacle),
           under_odds_sharp_closing = as.numeric(closing_under_odds_pinnacle),
           is_valid_result = TRUE,
           is_location_home = TRUE,
           is_replay = FALSE,
           is_empty_stadium = FALSE,
           leg = 0,
           match_id = create_match_id(competition_id, match_date, home_team, away_team)) %>%
    mutate(max_home_odds_betbrain = as.numeric(max_home_odds_betbrain),
           max_draw_odds_betbrain = as.numeric(max_draw_odds_betbrain),
           max_away_odds_betbrain = as.numeric(max_away_odds_betbrain),
           max_over_odds_betbrain = as.numeric(max_over_odds_betbrain),
           max_under_odds_betbrain = as.numeric(max_under_odds_betbrain),           
           max_home_odds_market = as.numeric(max_home_odds_market),
           max_draw_odds_market = as.numeric(max_draw_odds_market),
           max_away_odds_market = as.numeric(max_away_odds_market),
           max_over_odds_market = as.numeric(max_over_odds_market),
           max_under_odds_market = as.numeric(max_under_odds_market),           
           home_odds_max = case_when(!is.na(max_home_odds_betbrain) ~ max_home_odds_betbrain,
                                     !is.na(max_home_odds_market) ~ max_home_odds_market,
                                     !is.na(home_odds_max_data) ~ home_odds_max_data),
           draw_odds_max = case_when(!is.na(max_draw_odds_betbrain) ~ max_draw_odds_betbrain,
                                     !is.na(max_draw_odds_market) ~ max_draw_odds_market,
                                     !is.na(draw_odds_max_data) ~ draw_odds_max_data),          
           away_odds_max = case_when(!is.na(max_away_odds_betbrain) ~ max_away_odds_betbrain,
                                     !is.na(max_away_odds_market) ~ max_away_odds_market,
                                     !is.na(away_odds_max_data) ~ away_odds_max_data),
           over_odds_max = case_when(!is.na(max_over_odds_betbrain) ~ max_over_odds_betbrain,
                                     !is.na(max_over_odds_market) ~ max_over_odds_market,
                                     !is.na(over_odds_max_data) ~ over_odds_max_data),           
           under_odds_max = case_when(!is.na(max_under_odds_betbrain) ~ max_under_odds_betbrain,
                                      !is.na(max_under_odds_market) ~ max_under_odds_market,
                                      !is.na(under_odds_max_data) ~ under_odds_max_data)) %>%                                     
    select(match_id, competition_id, season_id, match_date, home_team, away_team, result, home_goals, away_goals,
           over_under_2_5, total_goals, home_odds_max, draw_odds_max, away_odds_max, over_odds_max, under_odds_max,
           home_odds_sharp_closing, draw_odds_sharp_closing, away_odds_sharp_closing, over_odds_sharp_closing, 
           under_odds_sharp_closing, is_valid_result, is_location_home, is_replay, leg, is_empty_stadium)

    
  
    return (results_main)
  
}

#' Calculate Max Odds
#' 
#' Helper Function. Calculate the maximum odds columns from columns specified by starts_with
#' 
#' @param x the tibble
#' @param col_names_starts_with the columns to extract with starts_with e.g. "home_odds"
#' @return maximum odds vector

calc_max_odds <- function(x, col_names_starts_with) {

  x <- select(x, starts_with(col_names_starts_with))
  
  # If no columns that start with that just return NAs
  
  if (ncol(x) == 0) {

    return(rep(NA_real_, nrow(x)))
    
  }
  
  x <- select_if(x, ~any(!is.na(.)))
  
  # If no columns which arent just all NAs then just return NAs
  
  if (ncol(x) == 0) {
    
    return(rep(NA_real_, nrow(x)))
    
  }
  
  max_odds <- x %>%
    mutate_all(as.numeric) %>%
    row_max(append_col = FALSE)
  
  return(max_odds)
  
}


#' Create Database Table Results Main Football Data Extra Leagues
#' 
#' Creates the data frame in the format required by the results main table in the live football database for extra 
#' leagues from football-data.co.uk
#' 
#' @param file filename of the raw football match data from football-data.co.uk
#' 
#' @return results main tibble ready for pushing to database

create_db_table_results_main_fd_extra <- function (file) {
  
  if (str_detect(file, "_all") == FALSE) {
    
    stop ("expected _all in filename")
    
  }
  
  if (str_ends(file, ".csv") == FALSE & str_ends(file, ".txt") == FALSE) {
    
    stop ("expected .csv or .txt in filen")
    
  }
  
  competition_id <- file %>%
    str_split(pattern = "/") %>%
    unlist() %>%
    rev() %>%
    magrittr::extract(1) %>%
    str_replace(pattern = "_all.csv", replacement = "")
  
  col_names_lookup <- tribble(
    ~dirty,       ~clean,
    # ***********/ ***********
    
    "Date",       "match_date",
    "Season",     "season_id",
    "Home",       "home_team",
    "Away",       "away_team",
    "HG",       "home_goals",
    "AG",       "away_goals",
    "GBH",        "home_odds_gamebooker",
    "GBD",        "draw_odds_gamebooker",
    "GBA",        "away_odds_gamebooker",
    "IWH",        "home_odds_interwetten",
    "IWD",        "draw_odds_interwetten",
    "IWA",        "away_odds_interwetten",
    "SBH",        "home_odds_sportingbet",
    "SBD",        "draw_odds_sportingbet",
    "SBA",        "away_odds_sportingbet",
    "WHH",        "home_odds_williamhill",     
    "WHD",        "draw_odds_williamhill",     
    "WHA",        "away_odds_williamhill",      
    "SYH",        "home_odds_stanleybet",     
    "SYD",        "draw_odds_stanleybet",      
    "SYA",        "away_odds_stanleybet",   
    "B365H",      "home_odds_bet365",
    "B365D",      "draw_odds_bet365",
    "B365A",      "away_odds_bet365",
    "GB>2.5",     "over_odds_gamebooker",
    "GB<2.5",     "under_odds_gamebooker",
    "BWH",        "home_odds_betandwin",      
    "BWD",        "draw_odds_betandwin",        
    "BWA",        "away_odds_betandwin",        
    "LBH",        "home_odds_ladbrokes",        
    "LBD",        "draw_odds_ladbrokes",
    "LBA",        "away_odds_ladbrokes",  
    "SJH",        "home_odds_stanjames",
    "SJD",        "draw_odds_stanjames",
    "SJA",        "away_odds_stanjames",
    "VCH",        "home_odds_vcbet",
    "VCD",        "draw_odds_vcbet",
    "VCA",        "away_odds_vcbet",
    "BbMxH",      "max_home_odds_betbrain",
    "BbMxD",      "max_draw_odds_betbrain",
    "BbMxA",      "max_away_odds_betbrain",
    "BbMx>2.5",   "max_over_odds_betbrain",
    "BbMx<2.5",   "max_under_odds_betbrain",
    "BSH",        "home_odds_bluesquare",
    "BSD",        "draw_odds_bluesquare",
    "BSA",        "away_odds_bluesquare",
    "PSCH",       "closing_home_odds_pinnacle",   
    "PSCD",       "closing_draw_odds_pinnacle",  
    "PSCA",       "closing_away_odds_pinnacle",  
    "B365>2.5",   "over_odds_bet365",
    "B365<2.5",   "under_odds_bet365",
    "PC>2.5",     "closing_over_odds_pinnacle",
    "PC<2.5",     "closing_under_odds_pinnacle",
    "SOH",        "home_odds_sportingodds",
    "SOD",        "draw_odds_sportingodds",
    "SOA",        "away_odds_sportingodds",
    "MaxH",       "max_home_odds_market",
    "MaxD",       "max_draw_odds_market",
    "MaxA",       "max_away_odds_market",
    "Max>2.5",    "max_over_odds_market",
    "Max<2.5",    "max_under_odds_market"
  )
  
  # Read in the dataset
  
  suppressWarnings(
    suppressMessages(
      fd_raw <- read_csv_robust(file)
    )
  )
  
  if (fd_raw$error == TRUE) {
    
    stop(fd_raw$error_message)
    
  } else {
    
    fd_raw <- fd_raw$csv
    
  }
  
  col_names_in_raw <- subset(col_names_lookup$dirty, col_names_lookup$dirty %in% colnames(fd_raw))
  
  fd_clean_names <- fd_raw %>%
    select(all_of(col_names_in_raw)) %>%
    set_colnames(subset(col_names_lookup$clean,
                        col_names_lookup$dirty %in% col_names_in_raw)) %>%
    filter(!is.na(match_date))
  
  col_names_not_in_raw <- subset(col_names_lookup$clean,
                                 !(col_names_lookup$dirty %in% colnames(fd_raw)))
  
  if (length(col_names_not_in_raw) > 0) {
    
    fd_clean_names <- NA %>%
      matrix(nrow = nrow(fd_clean_names), ncol = length(col_names_not_in_raw)) %>%
      set_colnames(col_names_not_in_raw) %>%
      as_tibble() %>%
      bind_cols(fd_clean_names, .)
    
  }
  
  results_main <- fd_clean_names %>%
    mutate(match_date = dmy(match_date),
           season_id = str_replace(season_id, "/", "_"),
           competition_id = competition_id,
           home_odds_max_data = calc_max_odds(fd_clean_names, "home_odds"),
           draw_odds_max_data = calc_max_odds(fd_clean_names, "draw_odds"),
           away_odds_max_data = calc_max_odds(fd_clean_names, "away_odds"),
           over_odds_max_data = calc_max_odds(fd_clean_names, "over_odds"),
           under_odds_max_data = calc_max_odds(fd_clean_names, "under_odds"),
           result = case_when(home_goals > away_goals ~ "home",
                              home_goals == away_goals ~ "draw",
                              home_goals < away_goals ~ "away",
                              TRUE ~ NA_character_),
           total_goals = home_goals + away_goals,
           over_under_2_5 = case_when(
             total_goals < 2.5 ~ "under",
             total_goals > 2.5 ~ "over",
             TRUE ~ NA_character_
           ),
           home_odds_sharp_closing = as.numeric(closing_home_odds_pinnacle),
           draw_odds_sharp_closing = as.numeric(closing_draw_odds_pinnacle),
           away_odds_sharp_closing = as.numeric(closing_away_odds_pinnacle),
           over_odds_sharp_closing = as.numeric(closing_over_odds_pinnacle),
           under_odds_sharp_closing = as.numeric(closing_under_odds_pinnacle),
           is_valid_result = TRUE,
           is_location_home = TRUE,
           is_replay = FALSE,
           is_empty_stadium = FALSE,
           leg = 0,
           match_id = create_match_id(competition_id, match_date, home_team, away_team)) %>%
    mutate(max_home_odds_betbrain = as.numeric(max_home_odds_betbrain),
           max_draw_odds_betbrain = as.numeric(max_draw_odds_betbrain),
           max_away_odds_betbrain = as.numeric(max_away_odds_betbrain),
           max_over_odds_betbrain = as.numeric(max_over_odds_betbrain),
           max_under_odds_betbrain = as.numeric(max_under_odds_betbrain),
           max_home_odds_market = as.numeric(max_home_odds_market),
           max_draw_odds_market = as.numeric(max_draw_odds_market),
           max_away_odds_market = as.numeric(max_away_odds_market),
           max_over_odds_market = as.numeric(max_over_odds_market),
           max_under_odds_market = as.numeric(max_under_odds_market),
           home_odds_max = case_when(!is.na(max_home_odds_betbrain) ~ max_home_odds_betbrain,
                                     !is.na(max_home_odds_market) ~ max_home_odds_market,
                                     !is.na(home_odds_max_data) ~ home_odds_max_data),
           draw_odds_max = case_when(!is.na(max_draw_odds_betbrain) ~ max_draw_odds_betbrain,
                                     !is.na(max_draw_odds_market) ~ max_draw_odds_market,
                                     !is.na(draw_odds_max_data) ~ draw_odds_max_data),
           away_odds_max = case_when(!is.na(max_away_odds_betbrain) ~ max_away_odds_betbrain,
                                     !is.na(max_away_odds_market) ~ max_away_odds_market,
                                     !is.na(away_odds_max_data) ~ away_odds_max_data),
           over_odds_max = case_when(!is.na(max_over_odds_betbrain) ~ max_over_odds_betbrain,
                                     !is.na(max_over_odds_market) ~ max_over_odds_market,
                                     !is.na(over_odds_max_data) ~ over_odds_max_data),
           under_odds_max = case_when(!is.na(max_under_odds_betbrain) ~ max_under_odds_betbrain,
                                      !is.na(max_under_odds_market) ~ max_under_odds_market,
                                      !is.na(under_odds_max_data) ~ under_odds_max_data)) %>%
    select(match_id, competition_id, season_id, match_date, home_team, away_team, result, home_goals, away_goals,
           over_under_2_5, total_goals, home_odds_max, draw_odds_max, away_odds_max, over_odds_max, under_odds_max,
           home_odds_sharp_closing, draw_odds_sharp_closing, away_odds_sharp_closing, over_odds_sharp_closing,
           under_odds_sharp_closing, is_valid_result, is_location_home, is_replay, leg, is_empty_stadium)

  return (results_main)
  
}








