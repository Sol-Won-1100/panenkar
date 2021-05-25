
## TO DO - consistency in the naming conventions here, functions sho of the format error_check_function_name

# Error checking function for checking results data set is in the correct format. THe exact columns required from
# results varies depending on the model / use case. Pass these as a character vector through expected_cols.

check_arg_results <- function (results, expected_cols, min_rows = 1) {
  
  if (!is.data.frame(results)) {
    
    stop("'results' must be a tibble or data.frame")
    
  }
  
  if (sum(expected_cols %in% colnames(results)) != length(expected_cols)) {
    
    stop (glue("'results' must have cols {glue_collapse(expected_cols, sep = ', ', last = ' and ')}."))
    
  }
  
  if ("match_date" %in% expected_cols) {
    
    if (class(results$match_date) != "Date") {
      
      stop("'results' column 'match_date' must be of class Date.")
      
    }
    
  }
  
  if (nrow(results) < min_rows) stop("results must have rows")
  
  return(results)
  
}


# Error checking function for checking xi parameter in the poisson time weight function. This is a cross cutting v
# variable used in multiple model so stored here.

check_arg_xi <- function (xi) {
  
  if (length(xi) != 1) {
    
    stop (glue("'xi' must be of length 1, not {length(xi)}."))
    
  }
  
  if (!is.numeric(xi)) {
    
    stop ("'xi' must be of class numeric.")
    
  }  
  
  return(xi)
  
}

# Current date used throughout the models for working with time weights. Must be a date of length 1.

check_arg_current_date <- function (current_date) {
  
  if (length(current_date) != 1) {
    
    stop (glue("'current_date' must be of length 1, not {length(current_date)}."))
    
  }
  
  if (!is.Date(current_date)) {
    
    stop ("'current_date' must be of class Date.")
    
  }
  
  return(current_date)
  
}

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


#' Check Season ID
#'
#' Check that season_id is in a correct format
#'
#' @param season_id the season_id to check
#'
#' @return a list, first element is TRUE or FALSE telling you if the argument was a season_is, the 2nd is the error
#' message
#' 
#' @note this is intended as helper function for verifying arguments 

is_season_id <- function (season_id) {
  
  if (!is.character(season_id)) {
    
    return(FALSE)
    
  }
  
  num_ids <- length(season_id)
  
  if (num_ids > 1) {
    
    warning_message <- paste("'season_id' has length", num_ids, ", expected 1. Returning 'FALSE'.")
    warning(warning_message)
    
    return(FALSE)
    
  }
  
  
  season_id <- season_id[!is.null(season_id)]
  season_id <- season_id[!is.na(season_id)]
  
  if (length(season_id) == 0) {
    
    return(FALSE)
    
  }
  
  min_possible_season <- 1800
  max_possible_season <- 2150
  
  
  if (nchar(season_id) == 4) {
    
    suppressWarnings(
      season_id <- as.numeric(season_id)      
    )
    
    if (is.na(season_id)) return(FALSE) 
    
    if (season_id < min_possible_season) return(FALSE)
    
    if (season_id > max_possible_season) return(FALSE)
    
    return(TRUE)
    
  } else if (nchar(season_id) == 9) {
    
    if (str_detect(season_id, "[:digit:]{4}_[:digit:]{4}") == FALSE) return(FALSE)
    
    start_year <- season_id %>% str_sub(1, 4) %>% as.numeric()
    end_year <- season_id %>% str_sub(6, 9) %>% as.numeric()
    
    if(start_year < min_possible_season) return(FALSE)
    if(start_year > max_possible_season) return(FALSE)
    if(end_year - start_year != 1) return(FALSE)
    
    return(TRUE)
    
  } else {
    
    return(FALSE)
    
  }
  
}


#' Check Season ID
#'
#' Check that season_id is in a correct format
#'
#' @param season_id the season_id to check
#'
#' @return a list, first element is TRUE or FALSE telling you if the argument was a season_is, the 2nd is the error
#' message
#' 
#' @note this is intended as helper function for verifying arguments 

error_check_season_id <- function (season_id) {
  
  if (!is.character(season_id)) {
    
    return(FALSE)
    
  }
  
  num_ids <- length(season_id)
  
  if (num_ids > 1) {
    
    warning_message <- paste("'season_id' has length", num_ids, ", expected 1. Returning 'FALSE'.")
    warning(warning_message)
    
    return(FALSE)
    
  }
  
  
  season_id <- season_id[!is.null(season_id)]
  season_id <- season_id[!is.na(season_id)]
  
  if (length(season_id) == 0) {
    
    return(FALSE)
    
  }
  
  min_possible_season <- 1800
  max_possible_season <- 2150
  
  
  if (nchar(season_id) == 4) {
    
    suppressWarnings(
      season_id <- as.numeric(season_id)      
    )
    
    if (is.na(season_id)) return(FALSE) 
    
    if (season_id < min_possible_season) return(FALSE)
    
    if (season_id > max_possible_season) return(FALSE)
    
    return(TRUE)
    
  } else if (nchar(season_id) == 9) {
    
    if (str_detect(season_id, "[:digit:]{4}_[:digit:]{4}") == FALSE) return(FALSE)
    
    start_year <- season_id %>% str_sub(1, 4) %>% as.numeric()
    end_year <- season_id %>% str_sub(6, 9) %>% as.numeric()
    
    if(start_year < min_possible_season) return(FALSE)
    if(start_year > max_possible_season) return(FALSE)
    if(end_year - start_year != 1) return(FALSE)
    
    return(TRUE)
    
  } else {
    
    return(FALSE)
    
  }
  
}

