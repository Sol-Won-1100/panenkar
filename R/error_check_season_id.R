
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