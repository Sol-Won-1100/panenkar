
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
  
  
  col_names_dirty <- c("Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR",
                       "HTHG", "HTAG", "HTR", "HS", "AS", "HST", "AST", "HFKC",
                       "AFKC", "HC", "AC", "HY", "AY", "HR", "AR")
  
  col_names_clean <- c("match_date", "home_team", "away_team", "home_goals",
                       "away_goals", "match_result", "home_goals_half1",
                       "away_goals_half1", "match_result_half1", "home_shots",
                       "away_shots", "home_shots_on_target",
                       "away_shots_on_target", "home_freekicks_conceded",
                       "away_freekicks_conceded", "home_corners",
                       "away_corners", "home_yellows", "away_yellows",
                       "home_reds", "away_reds")
  
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
  
  col_names_in_raw <- subset(col_names_dirty,
                             col_names_dirty %in% colnames(fd_raw))
  
  fd_clean_names <- fd_raw %>%
    select(col_names_in_raw) %>%
    set_colnames(subset(col_names_clean,
                        col_names_dirty %in% col_names_in_raw)) %>%
    filter(!is.na(match_date))
  
  # If some of the expected match data columns are missing fill them in with
  # NAs and bind to the dataset
  
  col_names_not_in_raw <- subset(col_names_clean,
                                 !(col_names_dirty %in% colnames(fd_raw)))
  
  if(length(col_names_not_in_raw) > 0){
    
    fd_clean_names <- NA %>%
      matrix(nrow = nrow(fd_clean_names), ncol = length(col_names_not_in_raw)) %>%
      set_colnames(col_names_not_in_raw) %>%
      as_tibble() %>%
      bind_cols(fd_clean_names, .)
    
  }
  
  # Now final cleaning
  fd_clean <- fd_clean_names %>%
    mutate(match_date = dmy(match_date),
           match_time = NA,
           home_goals_half2 = home_goals - home_goals_half1,
           away_goals_half2 = away_goals - away_goals_half1,
           match_result = result(home_goals, away_goals),
           match_result_half1 = result(home_goals_half1, away_goals_half1),
           match_result_half2 = result(home_goals_half2, away_goals_half2),
           season_id = season_id,
           competition_id = competition_id,
           match_id = paste0(competition_id, ".",
                             match_date, ".",
                             home_team, ".",
                             away_team)) %>%
    select(match_id, competition_id, season_id, match_date, match_time,
           home_team, away_team, home_goals, away_goals, match_result,
           home_goals_half1, away_goals_half1, match_result_half1,
           home_goals_half2, away_goals_half2, match_result_half2, home_shots,
           away_shots, home_shots_on_target, away_shots_on_target,
           home_freekicks_conceded, away_freekicks_conceded, home_corners,
           away_corners, home_yellows, away_yellows, home_reds, away_reds)
  
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
    extract(1) %>%
    str_replace(pattern = "_all.csv", replacement = "")
  
  col_names_dirty <- c("Date" , "Time", "Home", "Away", "HG", "AG", "Res",
                       "Season")
  
  col_names_clean <- c("match_date", "match_time", "home_team", "away_team", "home_goals",
                       "away_goals", "match_result", "season_id")
  
  col_names_clean_other <- c("home_goals_half1",
                             "away_goals_half1",
                             "match_result_half1",
                             "home_shots",
                             "away_shots",
                             "home_shots_on_target",
                             "away_shots_on_target",
                             "home_freekicks_conceded",
                             "away_freekicks_conceded",
                             "home_corners",
                             "away_corners",
                             "home_yellows",
                             "away_yellows",
                             "home_reds",
                             "away_reds")
  
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
  
  fd_clean_names <- fd_raw %>%
    select(col_names_dirty) %>%
    set_colnames(col_names_clean) %>%
    filter(!is.na(match_date))
  
  # If some of the expected match data columns are missing fill them in with
  # NAs and bind to the dataset
  
  fd_clean_names_with_nas <- NA %>%
    matrix(nrow = nrow(fd_clean_names), ncol = length(col_names_clean_other)) %>%
    set_colnames(col_names_clean_other) %>%
    as_tibble() %>%
    bind_cols(fd_clean_names, .)
  
  
  # Now final cleaning
  fd_clean <- fd_clean_names_with_nas %>%
    mutate(match_date = dmy(match_date),
           home_goals_half2 = home_goals - home_goals_half1,
           away_goals_half2 = away_goals - away_goals_half1,
           match_result = result(home_goals, away_goals),
           match_result_half1 = result(home_goals_half1, away_goals_half1),
           match_result_half2 = result(home_goals_half2, away_goals_half2),
           season_id = str_replace(season_id, "/", "_"),
           competition_id = competition_id,
           match_id = paste0(competition_id, ".",
                             match_date, ".",
                             home_team, ".",
                             away_team)) %>%
    select(match_id, competition_id, season_id, home_team, away_team,
           home_goals, away_goals, match_result, home_goals_half1,
           away_goals_half1, match_result_half1, home_goals_half2,
           away_goals_half2, match_result_half2, home_shots, away_shots,
           home_shots_on_target, away_shots_on_target, home_freekicks_conceded,
           away_freekicks_conceded, home_corners, away_corners, home_yellows,
           away_yellows, home_reds, away_reds)
  
  return(fd_clean)
  
}
