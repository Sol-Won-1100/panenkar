
#' Sequence Seasons
#'
#' Create a sequence of seasons between start and end seasons
#'
#' @param start_season a season_id
#' @param end_season a season_id must come after start_season
#' @return A sequence of seasons between and including the start and end seasons.
#' @export
#' @examples
#' sequence_seasons("2015_2016", "2019_2020")
#' sequence_seasons(2000, 2008)

sequence_seasons <- function(start_season, end_season){
 
  if (is_season_id(start_season) == FALSE) {
    
    stop("'start_season' must be a season_id")
    
  } 
  
  if (is_season_id(end_season) == FALSE) {
    
    stop("'end_season' must be a season_id")
    
  } 
  
  num_chars <- nchar(start_season)
  
  if (num_chars != nchar(end_season)) {
    
    stop("'start_season' and 'end_season' must in the same format, single or multi year")
    
  }
  
  if (num_chars == 4) {
    
    season_format <- "single_year"
    
  } else {
    
    season_format <- "multiple_years"
    
  }
  
  
  if (season_format == "multiple_years") {
    
    start_season1 <- start_season %>%
      str_sub(start = 1, end = 4) %>%
      as.numeric()
    
    end_season1 <- end_season %>%
      str_sub(start = 1, end = 4) %>%
      as.numeric()
    
    if (start_season1 > end_season1) {
      
      stop("'start_season' must come before or be the same as 'end_season'")
      
    }
    
    seasons1 <- start_season1:end_season1
    
    start_season2 <- start_season %>%
      str_sub(start = 6, end = 9) %>%
      as.numeric()
    
    end_season2 <- end_season %>%
      str_sub(start = 6, end = 9) %>%
      as.numeric()
    
    seasons2 <- start_season2:end_season2
    
    seasons <- paste0(seasons1, "_", seasons2)
    
  } else {
    
    start_season <- as.numeric(start_season)
    end_season <- as.numeric(end_season)
    
    if (start_season > end_season) {
      
      stop("'start_season' must come before or be the same as 'end_season'")      
      
    }
    
    seasons <- as.character(start_season:end_season)
    
  }
  
  return(seasons)
  
}


#' Minimum Season
#'
#' Determine the minimum season from a vector of season ids
#'
#' @param season_ids season_id in one of two formats, either a single 4 digit number representing the year or a 
#' character string in the format 1999_2000 to represent seasons which extend over successive years, typical in European
#' football.
#' @param remove_na should NAs be ignored, default TRUE
#' @return The minimum season_id
#' @export

min_season <- function (season_ids, remove_na = TRUE) f_season(season_ids, remove_na, f = min)

#' Maximum Season
#'
#' Determine the maximum season from a vector of season ids
#'
#' @param season_ids season_id in one of two formats, either a single 4 digit number representing the year or a 
#' character string in the format 1999_2000 to represent seasons which extend over successive years, typical in European
#' football.
#' @param remove_na should NAs be ignored, default TRUE
#' @return The minimum season_id
#' @export

max_season <- function (season_ids, remove_na = TRUE) f_season(season_ids, remove_na, f = max)

#' F Season
#'
#' Helper for min and max season functions. The code is the same expect the call to the min or max function so to avoid
#' duplication the functions min or max are passed to the f_season function.
#'
#' @param season_ids season_id in one of two formats, either a single 4 digit number representing the year or a 
#' character string in the format 1999_2000 to represent seasons which extend over successive years, typical in European
#' football.
#' @param remove_na should NAs be ignored, default TRUE
#' @param f the function to use on the season_ids, min or max

f_season <- function (season_ids, remove_na, f){

  num_season_ids <- length(season_ids)
  
  season_ids <- season_ids[map_lgl(season_ids, is_season_id)] # Only retain season_ids
  
  if (length(season_ids) < 1) {
    
    stop("bad 'season_ids'")
    
  }
  
  num_chars_id <- season_ids %>% nchar() %>% unique()
  
  if (length(num_chars_id) > 1) {
    
    stop("'season_ids' must be in a single format, either single year or multi year")
    
  }
  
  if (num_chars_id == 4){
    
    return(as.character(f(season_ids, na.rm = remove_na)))
    
  } else {
    
    season_ids_start <- season_ids %>% str_sub(1, 4) %>% as.numeric()
    index <- which(season_ids_start == f(season_ids_start, na.rm = remove_na))
    
    return(season_ids[index[1]])
  }
  
}

#' Previous Season
#'
#' Get previous season_id
#'
#' @param season_id season_id in form XXXX_XXXX or XXXX or a vector of season_ids
#' 
#' @return season_id
#' @export


previous_season <- function(season_ids) {

  num_season_ids <- length(season_ids)
  
  check_season_ids <- map_lgl(season_ids, is_season_id)
  
  season_ids_previous <- character(length = num_season_ids)
  
  for (i in seq_along(1:num_season_ids)) {
    
    if (check_season_ids[i] == FALSE) {
      
      season_ids_previous[i] <- NA_character_
      
    } else if (nchar(season_ids[i]) == 4) {
      
      season_ids_previous[i] <- as.character(as.numeric(season_ids[i]) - 1)
      
    } else if (nchar(season_ids[i]) == 9) {
      
      start_season <- season_ids[i] %>% str_sub(1, 4) %>% as.numeric()
      
      season_ids_previous[i] <- paste0(start_season - 1, "_", start_season)
      
    }
    
  }
  
  return(season_ids_previous)

}






