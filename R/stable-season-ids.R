

#' Sequence Seasons
#'
#' Create a sequence of seasons between start and end seasons
#'
#' @param start_season Takes two formats of season, either a single 4 digit
#' number representing the year or a character string in the format 1999_2000 to
#' represent seasons which extend over successive years, typical in European
#' football
#' @param end_season End season in the same format as start_season. Must come
#' after start_season
#' @return A sequence of seasons between and including the start and end
#' seasons.
#' @export
#' @examples
#' sequence_seasons("2015_2016", "2019_2020")
#' sequence_seasons(2000, 2008)

sequence_seasons <- function(start_season, end_season){
  
  ## Error handling
  
  if(class(start_season) != class(end_season)){
    stop("start_season and end_season must be both numeric or both character")
  }
  
  if(!is.character(start_season) & !is.numeric(start_season)){
    stop("start_season and end_season must be both numeric or both character")
  }
  
  if(is.character(start_season)){
    
    # Multiple years is a season which starts in one year n then finishes in
    # n + 1 otherwise season takes place within a single year
    
    season_format <- "multiple_years"
    
    if(nchar(start_season) != 9){
      stop("start_season must have nine characters in format YYYY_YYYY")
    }
    
    if(nchar(end_season) != 9){
      stop("end_season must have nine characters in format YYYY_YYYY")
    }
    
  } else {
    season_format <- "single_year"
    
    if(start_season > end_season){
      stop("start_season must come before or be the same as end_season")
    }
    
    
  }
  
  if(length(start_season) > 1){
    start_season <- start_season[1]
    warnings("length of start_season is > 1 so only first element will be used")
  }
  
  
  if(length(end_season) > 1){
    end_season <- end_season[1]
    warnings("length of end_season is > 1 so only first element will be used")
  }
  
  ## Sequence the years, depending on the format
  
  if(season_format == "multiple_years"){
    
    start_season1 <- start_season %>%
      str_sub(start = 1, end = 4) %>%
      as.numeric()
    
    end_season1 <- end_season %>%
      str_sub(start = 1, end = 4) %>%
      as.numeric()
    
    if(start_season1 > end_season1){
      stop("start_season must come before or be the same as end_season")
    }
    
    seasons1 <- start_season1:end_season1
    
    start_season2 <- start_season %>%
      str_sub(start = 6, end = 9) %>%
      as.numeric()
    
    end_season2 <- end_season %>%
      str_sub(start = 6, end = 9) %>%
      as.numeric()
    
    if(start_season2 > end_season2){
      stop("start_season must come before or be the same as end_season")
    }
    
    seasons2 <- start_season2:end_season2
    
    seasons <- paste0(seasons1, "_", seasons2)
    
  } else {
    
    if(season_format != "single_year"){
      warning("bad season_format specified, assuming single_year")
    }
    
    seasons <- start_season:end_season
  }
  
  return(seasons)
}



#' Maximum Season
#'
#' Determine the maximum season from a vector of season ids
#'
#' @param season_ids season_id in one of two formats, either a single 4 digit
#' number representing the year or a character string in the format 1999_2000 to
#' represent seasons which extend over successive years, typical in European
#' football.
#' @param remove_na should NAs be ignored
#' @return The maximum season_id
#' @export
#' @examples
#' seasons <- sequence_seasons("2015_2016", "2019_2020")
#' min_season(seasons)

max_season <- function(season_ids, remove_na = TRUE){
  
  if(is.numeric(season_ids)){
    return(max(season_ids, na.rm = remove_na))
  } else {
    season_ids_start <- season_ids %>% str_sub(1, 4) %>% as.numeric()
    index <- which(season_ids_start == max(season_ids_start, na.rm = remove_na))
    return(season_ids[index[1]])
  }
  
}




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



#' @title Previous Season
#'
#' @description Get previous season_id
#'
#' @param season_id season_id in form XXXX_XXXX or XXXX
#' 
#' @return season_id
#' @export

previous_season <- Vectorize(function(season_id){
  
  if(is.numeric(season_id)){
    season_id_previous <- season_id - 1
    
  } else {
    start_season <- season_id %>% str_sub(1, 4) %>% as.numeric()
    
    season_id_previous <- paste0(start_season - 1, "_", start_season)
  }
  
  return(season_id_previous)
})



#' Error Check Season ID
#'
#' Check that competition_id is in a correct format
#'
#' @param season_id See \link[panenkar]{sequence_seasons} for format details
#'
#' @return \code{TRUE} or \code{FALSE} indicating validity of the format of
#' season_id
#'
#' @examples
#'
#' .error_check_season_id("2018_2019") # Will return TRUE
#' .error_check_season_id("2018_2020") # FALSE
#' .error_check_season_id(2018) # TRUE

error_check_season_id <- function(season_id){
  
  if(is.character(season_id)){
    
    if(nchar(season_id) != 9){
      return(FALSE)
      
    }
    
    season_start_year <- str_sub(season_id, 1, 4) %>% as.numeric()
    season_end_year <- str_sub(season_id, 6, 9) %>% as.numeric()
    
    if(season_end_year - season_start_year != 1){
      return(FALSE)
      
    } else {
      return(TRUE)
      
    }
    
  } else if(is.numeric(season_id)){
    return(TRUE)
    
  } else {
    return(FALSE)
  }
}




