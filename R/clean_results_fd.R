
#' @title Clean results from football data
#'
#' @description Clean results data from www.football-data.co.uk
#'
#' @param file name of file downloaded from football-data.co.uk
#' @param data_type either "main_league" or "extra_league". See ?get_metadata
#' for more info on different types of league at www.football-data.co.uk.
#'
#' @export
#'
#' @examples
#' file <- "C:/Users/Neil/Documents/football-betting/data/raw/eng_pl_2018_2019"
#' clean_results(file)

clean_results_fd <- function(file, data_type = "main_league"){
  
  if(data_type == "main_league"){
    results <- clean_results_fdm(file)
  } else if(data_type == "extra_league"){
    results <- clean_results_fde(file)
  } else {
    stop("bad data_type supplied")
  }
  
  return(manual_clean_teams(results))
}


#' @title Clean results from football data main leagues
#'
#' @description Clean results data from www.football-data.co.uk main leagues
#'
#' @param file name of file downloaded from football-data.co.uk
#'
#' @export
#' @details Will change this to a helper function once more data sources added.
#'
#' @examples
#' file <- "C:/Users/Neil/Documents/football-betting/data/raw/eng_pl_2018_2019"
#' clean_results_fdm(file)
#' test

clean_results_fdm <- function(file){
  
  # First we clean the column names
  
  file_short <- str_split(file, pattern = "/") %>%
    unlist() %>% 
    rev() %>%
    magrittr::extract(1) %>%
    str_replace(pattern = ".csv", replacement = "")
  
  season_id <- file_short %>% str_extract(pattern = "[0-9]{4}_[0-9]{4}")
  
  competition_id <- str_replace(file_short, pattern = season_id,
                                replacement = "") %>%
    nchar() %>%
    subtract(1) %>%
    str_sub(file_short, 1, . )
  
  col_names_lookup <- tribble(
    ~dirty,       ~clean,
    # ***********/ ***********
    
    "Date",       "match_date",
    "HomeTeam",   "home_team",
    "AwayTeam",   "away_team",
    "FTHG",       "home_goals",
    "FTAG",       "away_goals",
    "HTHG",       "home_goals_half1",
    "HTAG",       "away_goals_half1",
    "GBH",        "odds_result_home_gamebooker",
    "GBD",        "odds_result_draw_gamebooker",
    "GBA",        "odds_result_away_gamebooker",
    "IWH",        "odds_result_home_interwetten",
    "IWD",        "odds_result_draw_interwetten",
    "IWA",        "odds_result_away_interwetten",
    "SBH",        "odds_result_home_sportingbet",
    "SBD",        "odds_result_draw_sportingbet",
    "SBA",        "odds_result_away_sportingbet",
    "WHH",        "odds_result_home_williamhill",     
    "WHD",        "odds_result_draw_williamhill",     
    "WHA",        "odds_result_away_williamhill",      
    "SYH",        "odds_result_home_stanleybet",     
    "SYD",        "odds_result_draw_stanleybet",      
    "SYA",        "odds_result_away_stanleybet",   
    "B365H",      "odds_result_home_bet365",
    "B365D",      "odds_result_draw_bet365",
    "B365A",      "odds_result_away_bet365",
    "GB>2.5",     "odds_ou25_over_gamebooker",
    "GB<2.5",     "odds_ou25_under_gamebooker",
    "GBAHH",      "odds_ah_home_gamebooker",
    "GBAHA",      "odds_ah_away_gamebooker",
    "GBAH",       "handicap_size_ah_home_gamebooker",
    "BWH",        "odds_result_home_betandwin",      
    "BWD",        "odds_result_draw_betandwin",        
    "BWA",        "odds_result_away_betandwin",        
    "LBH",        "odds_result_home_ladbrokes",        
    "LBD",        "odds_result_draw_ladbrokes",
    "LBA",        "odds_result_away_ladbrokes",  
    "B365AHH",    "odds_ah_home_bet365",
    "B365AHA",    "odds_ah_away_bet365",
    "B365AH",     "handicap_size_ah_home_bet365",
    "SJH",        "odds_result_home_stanjames",
    "SJD",        "odds_result_draw_stanjames",
    "SJA",        "odds_result_away_stanjames",
    "VCH",        "odds_result_home_vcbet",
    "VCD",        "odds_result_draw_vcbet",
    "VCA",        "odds_result_away_vcbet",
    "Bb1X2",      "num_bookmakers_result_betbrain",
    "BbMxH",      "odds_max_result_home_betbrain",
    "BbAvH",      "odds_average_result_home_betbrain",
    "BbMxD",      "odds_max_result_draw_betbrain",
    "BbAvD",      "odds_average_result_draw_betbrain",
    "BbMxA",      "odds_max_result_away_betbrain",
    "BbAvA",      "odds_average_result_away_betbrain",
    "BbOU",       "num_bookmakers_ou25_betbrain",
    "BbMx>2.5",   "odds_max_ou25_over_betbrain",
    "BbAv>2.5",   "odds_average_ou25_over_betbrain",
    "BbMx<2.5",   "odds_max_ou25_under_betbrain",
    "BbAv<2.5",   "odds_average_ou25_under_betbrain",
    "BbAH",       "num_bookmakers_ah_betbrain",
    "BbAHh",      "handicap_size_ah_home_betbrain",
    "BbMxAHH",    "odds_max_ah_home_betbrain",
    "BbAvAHH",    "odds_average_ah_home_betbrain",
    "BbMxAHA",    "odds_max_ah_away_betbrain",
    "BbAvAHA",    "odds_average_ah_away_betbrain",
    "BSH",        "odds_result_home_bluesquare",
    "BSD",        "odds_result_draw_bluesquare",
    "BSA",        "odds_result_away_bluesquare",
    "PSH",        "odds_result_home_pinnacle",
    "PSD",        "odds_result_draw_pinnacle",
    "PSA",        "odds_result_away_pinnacle",
    "PSCH",       "odds_closing_result_home_pinnacle",   
    "PSCD",       "odds_closing_result_draw_pinnacle",  
    "PSCA",       "odds_closing_result_away_pinnacle",  
    "HS",         "home_shots",
    "AS",         "away_shots",
    "HST",        "home_shots_on_target",
    "AST",        "away_shots_on_target",
    "HFKC",       "home_free_kicks_conceded",
    "AFKC",       "away_free_kicks_conceded",
    "HC",         "home_corners",
    "AC",         "away_corners",
    "HY",         "home_yellows",
    "AY",         "away_yellows",
    "HR",         "home_reds",
    "AR",         "away_reds",
    "Time",       "match_time",
    "HF",         "home_fouls_committed",
    "AF",         "away_fouls_committed",
    "MaxH",       "odds_max_result_home_market",
    "MaxD",       "odds_max_result_draw_market",
    "MaxA",       "odds_max_result_away_market",
    "AvgH",       "odds_average_result_home_market",
    "AvgD",       "odds_average_result_draw_market",
    "AvgA",       "odds_average_result_away_market",
    "B365>2.5",   "odds_ou25_over_bet365",
    "B365<2.5",   "odds_ou25_under_bet365",
    "P>2.5",      "odds_ou25_over_pinnacle",
    "P<2.5",      "odds_ou25_under_pinnacle",
    "Max>2.5",    "odds_max_ou25_over_market",
    "Max<2.5",    "odds_max_ou25_under_market",
    "Avg>2.5",    "odds_average_ou25_over_market",
    "Avg<2.5",    "odds_average_ou25_under_market",
    "AHh",        "handicap_size_ah_home_market",
    "PAHH",       "odds_ah_home_pinnacle",
    "PAHA",       "odds_ah_away_pinnacle",
    "MaxAHH",     "odds_max_ah_home_market",
    "MaxAHA",     "odds_max_ah_away_market",
    "AvgAHH",     "odds_average_ah_home_market",
    "AvgAHA",     "odds_average_ah_away_market",
    "B365CH",     "odds_closing_result_home_bet365",
    "B365CD",     "odds_closing_result_draw_bet365",
    "B365CA",     "odds_closing_result_away_bet365",
    "BWCH",       "odds_closing_result_home_betandwin",
    "BWCD",       "odds_closing_result_draw_betandwin",
    "BWCA",       "odds_closing_result_away_betandwin",
    "IWCH",       "odds_closing_result_home_interwetten",
    "IWCD",       "odds_closing_result_draw_interwetten",
    "IWCA",       "odds_closing_result_away_interwetten",
    "WHCH",       "odds_closing_result_home_williamhill",
    "WHCD",       "odds_closing_result_draw_williamhill",
    "WHCA",       "odds_closing_result_away_williamhill",
    "VCCH",       "odds_closing_result_home_vcbet",
    "VCCD",       "odds_closing_result_draw_vcbet",
    "VCCA",       "odds_closing_result_away_vcbet",
    "MaxCH",      "odds_closing_max_result_home_market",
    "MaxCD",      "odds_closing_max_result_draw_market",
    "MaxCA",      "odds_closing_max_result_away_market",
    "AvgCH",      "odds_closing_average_result_home_market",
    "AvgCD",      "odds_closing_average_result_draw_market",
    "AvgCA",      "odds_closing_average_result_away_market",
    "B365C>2.5",  "odds_closing_ou25_over_bet365",
    "B365C<2.5",  "odds_closing_ou25_under_bet365",
    "PC>2.5",     "odds_closing_ou25_over_pinnacle",
    "PC<2.5",     "odds_closing_ou25_under_pinnacle",
    "MaxC>2.5",   "odds_closing_max_ou25_over_market",
    "MaxC<2.5",   "odds_closing_max_ou25_under_market",
    "AvgC>2.5",   "odds_closing_average_ou25_over_market",
    "AvgC<2.5",   "odds_closing_average_ou25_under_market",
    "AHCh",       "handicap_size_closing_ah_home_market",
    "B365CAHH",   "odds_closing_ah_home_bet365",
    "B365CAHA",   "odds_closing_ah_away_bet365",
    "PCAHH",      "odds_closing_ah_home_pinnacle",
    "PCAHA",      "odds_closing_ah_away_pinnacle",
    "MaxCAHH",    "odds_closing_max_ah_home_market",
    "MaxCAHA",    "odds_closing_max_ah_away_market",
    "AvgCAHH",    "odds_closing_average_ah_home_market",
    "AvgCAHA",    "odds_closing_average_ah_away_market",
    "Attendance", "attendance",
    "Referee",    "referee",
    "HHW",        "home_hit_woodwork",
    "AHW",        "away_hit_woodwork",
    "HO",         "home_offsides",
    "AO",         "away_offsides",
    "HBP",        "home_booking_points",
    "ABP",        "away_booking_points",
    "SOH",        "odds_result_home_sportingodds",
    "SOD",        "odds_result_draw_sportingodds",
    "SOA",        "odds_result_away_sportingodds",
    "LBAHH",      "odds_ah_home_ladbrokes",
    "LBAHA",      "odds_ah_away_ladbrokes",
    "LBAH",       "num_bookmakers_ah_ladbrokes"
  )
  
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
  
  col_names_in_raw <- subset(col_names_lookup$dirty,
                             col_names_lookup$dirty %in% colnames(fd_raw))
  
  fd_clean_names <- fd_raw %>%
    select(col_names_in_raw) %>%
    set_colnames(subset(col_names_lookup$clean,
                        col_names_lookup$dirty %in% col_names_in_raw)) %>%
    filter(!is.na(match_date))
  
  # If some of the expected match data columns are missing fill them in with
  # NAs and bind to the dataset
  
  col_names_not_in_raw <- subset(col_names_lookup$clean,
                                 !(col_names_lookup$dirty %in% colnames(fd_raw)))
  
  if(length(col_names_not_in_raw) > 0){
    
    fd_clean_names <- NA %>%
      matrix(nrow = nrow(fd_clean_names), ncol = length(col_names_not_in_raw)) %>%
      set_colnames(col_names_not_in_raw) %>%
      as_tibble() %>%
      bind_cols(fd_clean_names, .)
    
  }
  
  # Cleaning and add time independent features to the dataset
  
  fd_clean <- fd_clean_names %>%
    mutate(match_date = dmy(match_date),
           season_id = season_id,
           competition_id = competition_id,
           num_bookmakers_ah_betbrain = as.numeric(num_bookmakers_ah_betbrain),
           handicap_size_ah_home_betbrain = as.numeric(handicap_size_ah_home_betbrain),
           odds_closing_result_home_pinnacle = as.numeric(odds_closing_result_home_pinnacle)) %>%
    distinct(.keep_all = TRUE) %>%
    add_independent_features_fd()
  
  
  return(fd_clean)
}


#' @title Clean results from football data extra leagues
#'
#' @description Clean results data from www.football-data.co.uk extra leagues
#'
#' @param file name of file downloaded from football-data.co.uk
#'
#' @export
#' @details Will change this to a helper function once more data sources added.
#'
#' @examples
#' file <- "C:/Users/Neil/Documents/football-betting/data/raw/eng_pl_2018_2019"
#' clean_results_fdm(file)

clean_results_fde <- function(file){
  
  # First we clean the column names
  
  competition_id <- file %>%
    str_split(pattern = "/") %>%
    unlist() %>%
    rev() %>%
    magrittr::extract(1) %>%
    str_replace(pattern = "_all.csv", replacement = "")
  
  col_names_lookup <- tribble(
    ~dirty,       ~clean,
    # **********/ ***********
    
    "Date",       "match_date",
    "Home",       "home_team",
    "Away",       "away_team",
    "HG",         "home_goals",
    "AG",         "away_goals",
    "HTHG",       "home_goals_half1",
    "HTAG",       "away_goals_half1",
    "GBH",        "odds_result_home_gamebooker",
    "GBD",        "odds_result_draw_gamebooker",
    "GBA",        "odds_result_away_gamebooker",
    "IWH",        "odds_result_home_interwetten",
    "IWD",        "odds_result_draw_interwetten",
    "IWA",        "odds_result_away_interwetten",
    "SBH",        "odds_result_home_sportingbet",
    "SBD",        "odds_result_draw_sportingbet",
    "SBA",        "odds_result_away_sportingbet",
    "WHH",        "odds_result_home_williamhill",     
    "WHD",        "odds_result_draw_williamhill",     
    "WHA",        "odds_result_away_williamhill",      
    "SYH",        "odds_result_home_stanleybet",     
    "SYD",        "odds_result_draw_stanleybet",      
    "SYA",        "odds_result_away_stanleybet",   
    "B365H",      "odds_result_home_bet365",
    "B365D",      "odds_result_draw_bet365",
    "B365A",      "odds_result_away_bet365",
    "GB>2.5",     "odds_ou25_over_gamebooker",
    "GB<2.5",     "odds_ou25_under_gamebooker",
    "GBAHH",      "odds_ah_home_gamebooker",
    "GBAHA",      "odds_ah_away_gamebooker",
    "GBAH",       "handicap_size_ah_home_gamebooker",
    "BWH",        "odds_result_home_betandwin",      
    "BWD",        "odds_result_draw_betandwin",        
    "BWA",        "odds_result_away_betandwin",        
    "LBH",        "odds_result_home_ladbrokes",        
    "LBD",        "odds_result_draw_ladbrokes",
    "LBA",        "odds_result_away_ladbrokes",  
    "B365AHH",    "odds_ah_home_bet365",
    "B365AHA",    "odds_ah_away_bet365",
    "B365AH",     "handicap_size_ah_home_bet365",
    "SJH",        "odds_result_home_stanjames",
    "SJD",        "odds_result_draw_stanjames",
    "SJA",        "odds_result_away_stanjames",
    "VCH",        "odds_result_home_vcbet",
    "VCD",        "odds_result_draw_vcbet",
    "VCA",        "odds_result_away_vcbet",
    "Bb1X2",      "num_bookmakers_result_betbrain",
    "BbMxH",      "odds_max_result_home_betbrain",
    "BbAvH",      "odds_average_result_home_betbrain",
    "BbMxD",      "odds_max_result_draw_betbrain",
    "BbAvD",      "odds_average_result_draw_betbrain",
    "BbMxA",      "odds_max_result_away_betbrain",
    "BbAvA",      "odds_average_result_away_betbrain",
    "BbOU",       "num_bookmakers_ou25_betbrain",
    "BbMx>2.5",   "odds_max_ou25_over_betbrain",
    "BbAv>2.5",   "odds_average_ou25_over_betbrain",
    "BbMx<2.5",   "odds_max_ou25_under_betbrain",
    "BbAv<2.5",   "odds_average_ou25_under_betbrain",
    "BbAH",       "num_bookmakers_ah_betbrain",
    "BbAHh",      "handicap_size_ah_home_betbrain",
    "BbMxAHH",    "odds_max_ah_home_betbrain",
    "BbAvAHH",    "odds_average_ah_home_betbrain",
    "BbMxAHA",    "odds_max_ah_away_betbrain",
    "BbAvAHA",    "odds_average_ah_away_betbrain",
    "BSH",        "odds_result_home_bluesquare",
    "BSD",        "odds_result_draw_bluesquare",
    "BSA",        "odds_result_away_bluesquare",
    "PSH",        "odds_result_home_pinnacle",
    "PSD",        "odds_result_draw_pinnacle",
    "PSA",        "odds_result_away_pinnacle",
    "PSCH",       "odds_closing_result_home_pinnacle",   
    "PSCD",       "odds_closing_result_draw_pinnacle",  
    "PSCA",       "odds_closing_result_away_pinnacle",  
    "HS",         "home_shots",
    "AS",         "away_shots",
    "HST",        "home_shots_on_target",
    "AST",        "away_shots_on_target",
    "HFKC",       "home_free_kicks_conceded",
    "AFKC",       "away_free_kicks_conceded",
    "HC",         "home_corners",
    "AC",         "away_corners",
    "HY",         "home_yellows",
    "AY",         "away_yellows",
    "HR",         "home_reds",
    "AR",         "away_reds",
    "Time",       "match_time",
    "HF",         "home_fouls_committed",
    "AF",         "away_fouls_committed",
    "MaxH",       "odds_max_result_home_market",
    "MaxD",       "odds_max_result_draw_market",
    "MaxA",       "odds_max_result_away_market",
    "AvgH",       "odds_average_result_home_market",
    "AvgD",       "odds_average_result_draw_market",
    "AvgA",       "odds_average_result_away_market",
    "B365>2.5",   "odds_ou25_over_bet365",
    "B365<2.5",   "odds_ou25_under_bet365",
    "P>2.5",      "odds_ou25_over_pinnacle",
    "P<2.5",      "odds_ou25_under_pinnacle",
    "Max>2.5",    "odds_max_ou25_over_market",
    "Max<2.5",    "odds_max_ou25_under_market",
    "Avg>2.5",    "odds_average_ou25_over_market",
    "Avg<2.5",    "odds_average_ou25_under_market",
    "AHh",        "handicap_size_ah_home_market",
    "PAHH",       "odds_ah_home_pinnacle",
    "PAHA",       "odds_ah_away_pinnacle",
    "MaxAHH",     "odds_max_ah_home_market",
    "MaxAHA",     "odds_max_ah_away_market",
    "AvgAHH",     "odds_average_ah_home_market",
    "AvgAHA",     "odds_average_ah_away_market",
    "B365CH",     "odds_closing_result_home_bet365",
    "B365CD",     "odds_closing_result_draw_bet365",
    "B365CA",     "odds_closing_result_away_bet365",
    "BWCH",       "odds_closing_result_home_betandwin",
    "BWCD",       "odds_closing_result_draw_betandwin",
    "BWCA",       "odds_closing_result_away_betandwin",
    "IWCH",       "odds_closing_result_home_interwetten",
    "IWCD",       "odds_closing_result_draw_interwetten",
    "IWCA",       "odds_closing_result_away_interwetten",
    "WHCH",       "odds_closing_result_home_williamhill",
    "WHCD",       "odds_closing_result_draw_williamhill",
    "WHCA",       "odds_closing_result_away_williamhill",
    "VCCH",       "odds_closing_result_home_vcbet",
    "VCCD",       "odds_closing_result_draw_vcbet",
    "VCCA",       "odds_closing_result_away_vcbet",
    "MaxCH",      "odds_closing_max_result_home_market",
    "MaxCD",      "odds_closing_max_result_draw_market",
    "MaxCA",      "odds_closing_max_result_away_market",
    "AvgCH",      "odds_closing_average_result_home_market",
    "AvgCD",      "odds_closing_average_result_draw_market",
    "AvgCA",      "odds_closing_average_result_away_market",
    "B365C>2.5",  "odds_closing_ou25_over_bet365",
    "B365C<2.5",  "odds_closing_ou25_under_bet365",
    "PC>2.5",     "odds_closing_ou25_over_pinnacle",
    "PC<2.5",     "odds_closing_ou25_under_pinnacle",
    "MaxC>2.5",   "odds_closing_max_ou25_over_market",
    "MaxC<2.5",   "odds_closing_max_ou25_under_market",
    "AvgC>2.5",   "odds_closing_average_ou25_over_market",
    "AvgC<2.5",   "odds_closing_average_ou25_under_market",
    "AHCh",       "handicap_size_closing_ah_home_market",
    "B365CAHH",   "odds_closing_ah_home_bet365",
    "B365CAHA",   "odds_closing_ah_away_bet365",
    "PCAHH",      "odds_closing_ah_home_pinnacle",
    "PCAHA",      "odds_closing_ah_away_pinnacle",
    "MaxCAHH",    "odds_closing_max_ah_home_market",
    "MaxCAHA",    "odds_closing_max_ah_away_market",
    "AvgCAHH",    "odds_closing_average_ah_home_market",
    "AvgCAHA",    "odds_closing_average_ah_away_market",
    "Attendance", "attendance",
    "Referee",    "referee",
    "HHW",        "home_hit_woodwork",
    "AHW",        "away_hit_woodwork",
    "HO",         "home_offsides",
    "AO",         "away_offsides",
    "HBP",        "home_booking_points",
    "ABP",        "away_booking_points",
    "SOH",        "odds_result_home_sportingodds",
    "SOD",        "odds_result_draw_sportingodds",
    "SOA",        "odds_result_away_sportingodds",
    "LBAHH",      "odds_ah_home_ladbrokes",
    "LBAHA",      "odds_ah_away_ladbrokes",
    "LBAH",       "num_bookmakers_ah_ladbrokes"
  )
  
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
  
  col_names_in_raw <- subset(col_names_lookup$dirty,
                             col_names_lookup$dirty %in% colnames(fd_raw))
  
  fd_clean_names <- fd_raw %>%
    select(col_names_in_raw) %>%
    set_colnames(subset(col_names_lookup$clean,
                        col_names_lookup$dirty %in% col_names_in_raw)) %>%
    filter(!is.na(match_date))
  
  # If some of the expected match data columns are missing fill them in with
  # NAs and bind to the dataset
  
  col_names_not_in_raw <- subset(col_names_lookup$clean,
                                 !(col_names_lookup$dirty %in% colnames(fd_raw)))
  
  if(length(col_names_not_in_raw) > 0){
    
    fd_clean_names <- NA %>%
      matrix(nrow = nrow(fd_clean_names), ncol = length(col_names_not_in_raw)) %>%
      set_colnames(col_names_not_in_raw) %>%
      as_tibble() %>%
      bind_cols(fd_clean_names, .)
    
  }
  
  # Cleaning and add time independent features to the dataset
  
  fd_clean <- fd_clean_names %>%
    mutate(match_date = dmy(match_date),
           season_id = str_replace(season_id, "/", "_"),
           competition_id = competition_id,
           num_bookmakers_ah_betbrain = as.numeric(num_bookmakers_ah_betbrain),
           handicap_size_ah_home_betbrain = as.numeric(handicap_size_ah_home_betbrain),
           odds_closing_result_home_pinnacle = as.numeric(odds_closing_result_home_pinnacle)) %>%
    distinct(.keep_all = TRUE) %>%
    add_independent_features_fd()
  
  
  return(fd_clean)
}







# clean_results_fde <- function(file){
#   
#   # First we clean the column names
#   
#   competition_id <- file %>%
#     str_split(pattern = "/") %>%
#     unlist() %>%
#     rev() %>%
#     magrittr::extract(1) %>%
#     str_replace(pattern = "_all.csv", replacement = "")
#   
#   col_names_dirty <- c("Date" , "Time", "Home", "Away", "HG", "AG", "Res",
#                        "Season")
#   
#   col_names_clean <- c("match_date", "match_time", "home_team", "away_team", "home_goals",
#                        "away_goals", "match_result", "season_id")
#   
#   col_names_clean_other <- c("home_goals_half1",
#                              "away_goals_half1",
#                              "match_result_half1",
#                              "home_shots",
#                              "away_shots",
#                              "home_shots_on_target",
#                              "away_shots_on_target",
#                              "home_freekicks_conceded",
#                              "away_freekicks_conceded",
#                              "home_corners",
#                              "away_corners",
#                              "home_yellows",
#                              "away_yellows",
#                              "home_reds",
#                              "away_reds")
#   
#   # Read in the dataset
#   suppressWarnings(
#     suppressMessages(
#       fd_raw <- read_csv_robust(file)
#     )
#   )
#   
#   if(fd_raw$error == TRUE){
#     stop(fd_raw$error_message)
#   } else {
#     fd_raw <- fd_raw$csv
#   }
#   
#   fd_clean_names <- fd_raw %>%
#     select(col_names_dirty) %>%
#     set_colnames(col_names_clean) %>%
#     filter(!is.na(match_date))
#   
#   # If some of the expected match data columns are missing fill them in with
#   # NAs and bind to the dataset
#   
#   fd_clean_names_with_nas <- NA %>%
#     matrix(nrow = nrow(fd_clean_names), ncol = length(col_names_clean_other)) %>%
#     set_colnames(col_names_clean_other) %>%
#     as_tibble() %>%
#     bind_cols(fd_clean_names, .)
#   
#   
#   # Now final cleaning
#   fd_clean <- fd_clean_names_with_nas %>%
#     mutate(match_date = dmy(match_date),
#            home_goals_half2 = home_goals - home_goals_half1,
#            away_goals_half2 = away_goals - away_goals_half1,
#            match_result = result(home_goals, away_goals),
#            match_result_half1 = result(home_goals_half1, away_goals_half1),
#            match_result_half2 = result(home_goals_half2, away_goals_half2),
#            season_id = str_replace(season_id, "/", "_"),
#            competition_id = competition_id,
#            match_id = paste0(competition_id, ".",
#                              match_date, ".",
#                              home_team, ".",
#                              away_team)) %>%
#     select(match_id, competition_id, season_id, match_date, home_team, away_team,
#            home_goals, away_goals, match_result, home_goals_half1,
#            away_goals_half1, match_result_half1, home_goals_half2,
#            away_goals_half2, match_result_half2, home_shots, away_shots,
#            home_shots_on_target, away_shots_on_target, home_freekicks_conceded,
#            away_freekicks_conceded, home_corners, away_corners, home_yellows,
#            away_yellows, home_reds, away_reds)
#   
#   return(fd_clean)
#   
# }
