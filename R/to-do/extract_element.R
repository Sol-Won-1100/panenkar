
#' @title Extract Element
#'
#' @description Extract an element/elements from a tibble/data.frame. Handy 
#' shortcut bolted on  dplyr manipulations when piping.
#'
#' @param x tibble or data.frame
#' @param rows vector of numbers specifying the row or rows of to be extracted
#' @param ... column name/names in x to extract using tidy evaluation
#'
#' @return 
#'
#' 
#'
#' @examples
#'
#' library(dplyr)
#' 
#' tb <- tibble(indices = 1:10, rand_nums = runif(10), 
#'              rand_letters = sample(letters, 10))
#' 
#' tb %>% 
#'   extract_element(5, rand_nums)

extract_element <- function(x, rows, ..., unlisted = TRUE){
  
  x <- x %>%
    slice(rows) %>%
    select(...) 
  
  if(unlisted == TRUE){
    x <- unlist(x)
  }
  
  return(x)
}



