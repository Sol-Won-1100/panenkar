
#' Row Maximum
#' 
#' Return the row maximum for columns ... in a tibble x using tidy evaluation
#' 
#' @param x a tibble
#' @param ... columns of the tibble to compute the rowwise maximum on, must be numeric
#' @param na_rm if TRUE ignore NAs if FALSE dont ignore
#' @param append_col if TRUE adds a column to the end of x with the maximum
#' @param col_name_max column name of the maximum, only used if append_col = TRUE, if left as name x_row_max used as
#' default column name 
#' 
#' @return a vector of maximums
#' 
#' @example 
#' 
#' library(tidyverse)
#' 
#' x <- tribble(~x1,    ~x2,      ~x3,
#'              "dog",   2,        4,
#'              "cat",   NA_real_, 5.5,
#'              "mouse", 40,       20)
#' 
#' row_max(x, x2, x3, col_name = "max_x2_x3")
#' row_max(x, x2, x3, append_col = FALSE)

row_max <- function (x, ..., na_rm = TRUE, append_col = TRUE, max_col_name = NA) {
  
  cols_quo <- enquos(...)
  
  cols_quo_string <- cols_quo %>% as.character() %>% str_replace_all("~", "")
 
  col_names_x <- colnames(x)
  
  if (length(cols_quo_string) > 0) {
    
    # Check specfied cols are in x
    
    check_cols <- cols_quo_string %in% col_names_x
    
    if(sum(check_cols) == 0){

      cols_quo_string[!check_cols] %>%
        paste(collapse = ",") %>%
        paste("not present in x") %>%
        stop()
      
    }
    
    x_subset <- select(x, !!!cols_quo)
    
  } else {
    
    x_subset <- x
    
  }
  
  # Check columns are numeric
  
  is_col_numeric <- map_lgl(x_subset, is.numeric)
  
  if (length(is_col_numeric) != sum(is_col_numeric)) {
    
    stop ("non-numeric column(s) of x specified in ...")
    
  }
  
  # Compute maximums
  
  x_matrix <- as.matrix(x_subset)
  
  x_row_max <- numeric(length = nrow(x_matrix))
  
  for (i in 1:nrow(x_matrix)) {
    
    x_row_max[i] <- max(x_matrix[i,], na.rm = na_rm)
    
  }
  
  if (append_col == TRUE) {
    
    if (is.na(max_col_name)) {
      
      max_col_name <- "x_row_max"
      
    }
    
    x[, max_col_name] <- x_row_max
    
    return (x)
    
  } else {
    
    return (x_row_max)
    
  }

}


#' Row Minimum
#' 
#' Return the row minimum for columns ... in a tibble x using tidy evaluation
#' 
#' @param x a tibble
#' @param ... columns of the tibble to compute the rowwise minimum on, must be numeric
#' @param na_rm if TRUE ignore NAs if FALSE dont ignore
#' @param append_col if TRUE adds a column to the end of x with the minimum
#' @param col_name_min column name of the minimum, only used if append_col = TRUE, if left as name x_row_min used as
#' default column name 
#' 
#' @return a vector of minimums
#' 
#' @example 
#' 
#' library(tidyverse)
#' 
#' x <- tribble(~x1,    ~x2,      ~x3,
#'              "dog",   2,        4,
#'              "cat",   NA_real_, 5.5,
#'              "mouse", 40,       20)
#' 
#' row_min(x, x2, x3, col_name = "min_x2_x3")
#' row_min(x, x2, x3, append_col = FALSE)

row_min <- function (x, ..., na_rm = TRUE, append_col = TRUE, min_col_name = NA) {
  
  cols_quo <- enquos(...)
  
  cols_quo_string <- cols_quo %>% as.character() %>% str_replace_all("~", "")
  
  col_names_x <- colnames(x)
  
  if (length(cols_quo_string) > 0) {
    
    # Check specfied cols are in x
    
    check_cols <- cols_quo_string %in% col_names_x
    
    if(sum(check_cols) == 0){
      
      cols_quo_string[!check_cols] %>%
        paste(collapse = ",") %>%
        paste("not present in x") %>%
        stop()
      
    }
    
    x_subset <- select(x, !!!cols_quo)
    
  } else {
    
    x_subset <- x
    
  }
  
  # Check columns are numeric
  
  is_col_numeric <- map_lgl(x_subset, is.numeric)
  
  if (length(is_col_numeric) != sum(is_col_numeric)) {
    
    stop ("non-numeric column(s) of x specified in ...")
    
  }
  
  # Compute minimums
  
  x_matrix <- as.matrix(x_subset)
  
  x_row_min <- numeric(length = nrow(x_matrix))
  
  for (i in 1:nrow(x_matrix)) {
    
    x_row_min[i] <- min(x_matrix[i,], na.rm = na_rm)
    
  }
  
  if (append_col == TRUE) {
    
    if (is.na(min_col_name)) {
      
      min_col_name <- "x_row_min"
      
    }
    
    x[, min_col_name] <- x_row_min
    
    return (x)
    
  } else {
    
    return (x_row_min)
    
  }
  
}