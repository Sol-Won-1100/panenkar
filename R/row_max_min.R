
#' Row Maximum / Minimum
#' 
#' Return the row maximum / minimum for columns ... in a tibble x using tidy evaluation
#' 
#' @inheritParams row_numeric_function
#' 
#' @note passes the max function to row_numeric_function which applys the max functions to rows
#' 
#' @examples 
#' 
#' 
#' library(tidyverse)
#' 
#' x <- tribble(~x1,    ~x2,      ~x3,
#'              "dog",   2,        4,
#'              "cat",   NA_real_, 5.5,
#'              "mouse", 40,       20)
#' 
#' row_max(x, x2, x3, new_col_name = "max_x2_x3")
#' row_min(x, x2, x3, append_col = FALSE)
#' @rdname row_max
#' @export

row_max <- function (...) row_numeric_function (..., f = "max")

#' @rdname row_max

row_min <- function (...) row_numeric_function (..., f = "min")

#' Row Numeric Function
#' 
#' Apply the function f to a tibble x rowwise
#' 
#' @param x a tibble
#' @param ... columns of the tibble to compute the rowwise maximum on, must be numeric
#' @param na_rm if TRUE ignore NAs if FALSE dont ignore
#' @param append_col if TRUE adds a column to the end of x with the maximum
#' @param new_col_name column name of the maximum, only used if append_col = TRUE, if left as name x_row_max used as
#' default column name 
#' @param f the function name as string e.g. "max" for row maxes
#' 
#' @return a vector 
#' @note in theory you could use a whole bunch of functions for f, have only written tests for max and min


row_numeric_function <- function(x, ..., na_rm = TRUE, append_col = TRUE, new_col_name = NA, f) {
  
  # Convert string to function name
  
  f_string <- f
  f <- get(f)
  
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
  
  # Compute max/min or whatever the function is
  
  x_matrix <- as.matrix(x_subset)
  
  x_row_f <- numeric(length = nrow(x_matrix))
  
  for (i in 1:nrow(x_matrix)) {
    
    # If row is all NAs fill with NA otherwise compute minimum
    
    if (sum(is.na(x_matrix[i,])) == ncol(x_matrix)) {
      
      x_row_f[i] <- NA_real_
      
    } else {
      
      x_row_f[i] <- f(x_matrix[i,], na.rm = na_rm)
      
    }
    
    
    
  }
  
  if (append_col == TRUE) {
    
    if (is.na(new_col_name)) {
      
      new_col_name <- paste0("x_row_", f_string)
      
    }
    
    x[, new_col_name] <- x_row_f
    
    return (x)
    
  } else {
    
    return (x_row_f)
    
  }
  
}


