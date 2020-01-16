
#' Error Check Competition ID
#'
#' Check that competition_id is in a correct format
#'
#' @param competition_id See \link[panenkar]{get_metadata} for format details
#'
#' @return \code{TRUE} or \code{FALSE} indicating validity of the format of
#' competition_id
#'
#' @examples
#'
#' error_check_competition_id("england_pl") # Will return FALSE
#' error_check_competition_id("eng_pl") # TRUE

error_check_competition_id <- function(competition_id){
  
  competition_ids_good <- get_metadata() %>% names()
  
  return(competition_id %in% competition_ids_good)
  
}
