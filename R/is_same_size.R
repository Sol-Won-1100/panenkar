

#' @title Is Same Size
#' @description Test if matrices, data.frames or tibbles, or a combination of these have the same number of rows and 
#'              columns
#' @param ... 
#' @return TRUE or FALSE
#' @details DETAILS
#' @examples 
#' 
#' probs <- matrix(runif(9), nrow = 3)
#' odds <- matrix(runif(9, min = 1, max = 25), nrow = 3)
#' outcomes <- factor(sample(c("home", "draw", "away"), size = 9, replace = TRUE), levels = c("home", "draw", "away"))
#' 
#' betting_stats <- simulate_bets(probs, odds, outcomes)
#'
#' @rdname is_same_size
#' @export 

is_same_size <- function(...) {
  
  params <- list(...)
  
  if (length(params) < 2) stop ("must supply at least 2 items")
  
  is_ok_structures <- map_lgl(params, ~ !is.matrix(.x) | !is.data.frame(.x))

  if (sum(is_ok_structures) != length(is_ok_structures)) stop ("arguments must be data.frames, matrices or tibbles")
  
  row_counts <- map_dbl(params, nrow)
  col_counts <- map_dbl(params, ncol)

  if (length(unique(row_counts)) != 1) {
    
    return(FALSE)
    
  }

  if (length(unique(col_counts)) != 1) {
    
    return(FALSE)
    
  }
  
  return(TRUE)
  
} 

