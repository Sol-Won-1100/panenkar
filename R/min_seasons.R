
#' Minimum Season
#'
#' Determine the minimum season from a vector of season ids
#'
#' @param season_ids season_id in one of two formats, either a single 4 digit
#' number representing the year or a character string in the format 1999_2000 to
#' represent seasons which extend over successive years, typical in European
#' football.
#' @param remove_na should NAs be ignored
#' @return The minimum season_id
#' @export
#' @examples
#' seasons <- sequence_seasons("2015_2016", "2019_2020")
#' min_season(seasons)

min_season <- function(season_ids, remove_na = TRUE){
  
  if(is.numeric(season_ids)){
    return(min(season_ids, na.rm = remove_na))
  } else {
    season_ids_start <- season_ids %>% str_sub(1, 4) %>% as.numeric()
    index <- which(season_ids_start == min(season_ids_start, na.rm = remove_na))
    return(season_ids[index[1]])
  }
  
}
