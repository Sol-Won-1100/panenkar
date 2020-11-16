
# Error checking function for checking results data set is in the correct format. THe exact columns required from
# results varies depending on the model / use case. Pass these as a character vector through expected_cols.

check_arg_results <- function (results, expected_cols) {
  
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
  
}

