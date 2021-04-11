
#' @title Remove Margin
#' @description Remove the bookmakers margin from a set of odds
#' @param x a vector, data.frame or matrix of decimal odds
#' @param method one of 'proportional' or 'straight'. Default: 'proportional'
#' @return OUTPUT_DESCRIPTION
#' @details the 'proportional' method detailed here: https://www.football-data.co.uk/The_Wisdom_of_the_Crowd_updated.pdf 
#' but can also be the 'straight' method. The  proportional method is thought to be superior, straight method is just 
#' divide everything by the over round.
#' @rdname remove_margin
#' @export 

remove_margin <- function(x, method = "proportional") {
  
  if(is.matrix(x)) {
    
    x <- as_tibble(x)
    
  } 
  
  
  if (is.data.frame(x)) {

    x_no_margin <- x %>% 
      mutate(id = 1:n()) %>%
      group_split(id, .keep = FALSE) %>%
      map(unlist) %>%
      map(remove_margin_vector, method) %>% 
      map(matrix, nrow = 1) %>% 
      map(as_tibble) %>% 
      bind_rows() %>%
      set_colnames(colnames(x))
      
  } else {
    
    x_no_margin <- remove_margin_vector(x, method)
    
  }
  
  return(x_no_margin)
  
}




#' @title Remove Margin Vector
#' @description Remove the bookmakers margin from a set of odds
#' @param x a vector of decimal odds
#' @param method one of 'proportional' or 'straight'. Default: 'proportional'
#' @return OUTPUT_DESCRIPTION
#' @details the 'proportional' method detailed here: https://www.football-data.co.uk/The_Wisdom_of_the_Crowd_updated.pdf 
#' but can also be the 'straight' method. The  proportional method is thought to be superior, straight method is just 
#' divide everything by the over round.

remove_margin_vector <- function(x, method = "proportional"){
  
  if (!is.character(method)) {
    
    stop("'method' must be of class character.")
    
  }
  
  if (length(method) != 1) {
    
    stop(glue("'method' must be of length 1 not {length(method)}."))
    
  }
  
  if (method != "proportional" & method != "straight") {
    
    stop(glue("'method' must be one of 'proportional' or 'straight' not '{method}'."))
    
  }
  
  if (!is.vector(x) ) {
    
    stop(glue("'x' must be a vector."))
    
  }
  
  if (length(x) == 1) {
    if (is.na(x)) {
      
      warning("'x' is NA, returning NA")
      return(NA_real_)
      
    }
  }

  if (!is.numeric(x)) {
    
    stop ("'x' must be numeric")
    
  }
  
  x_invalid <- x[!is.na(x)]
  
  if (length(x_invalid) != length(x)) {
    
    warning("'x' contains NAs, returning NAs")
    return(rep(NA_real_, length(x)))
    
  }
  
  x_lt1 <- x[x <= 1]

  if (length(x_lt1) > 0) {
    
    warning("elements of 'x' must be > 1 return NAs")
    return(rep(NA_real_, length(x)))
    
  }

  prob_with_margin <- 1 / x
    
  margin <- sum(prob_with_margin) - 1
  
  if (method == "straight") {
    
    prob_no_margin <- prob_with_margin / (1 + margin)
    
  } else  {
    
    # Proportional method
    
    num_selections <- length(x)
    prob_no_margin <- (prob_with_margin * num_selections - margin) / num_selections
    
  } 
  
}

