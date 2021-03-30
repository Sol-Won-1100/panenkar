
#' @title Remove Margin
#' @description Remove the bookmakers margin from a set of odds
#' @param x a vector of decimal odds
#' @param method one of 'proportional' or 'straight'. Default: 'proportional'
#' @return OUTPUT_DESCRIPTION
#' @details the 'proportional' method detailed here: https://www.football-data.co.uk/The_Wisdom_of_the_Crowd_updated.pdf 
#' but can also be the 'straight' method. The  proportional method is thought to be superior, straight method is just 
#' divide everything by the over round.
#' @rdname remove_margin
#' @export 

remove_margin <- function(x, method = "proportional"){
  
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

  if (!is.numeric(x)) {
    
    stop ("'x' must be numeric")
    
  }

  if (length(x) == 1) {
    if (is.na(x)) {
      
      stop ("'x' is NA")
      
    }
  }
  
  x_invalid <- x[!is.na(x)]
  
  if (length(x_invalid) != length(x)) {
    
    stop("all elements of 'x' must not be NA")
    
  }
  
  x_lt1 <- x[x <= 1]
  
  if (length(x_lt1) != length(x)) {
    
    stop("elements of 'x' must be > 1")
    
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

