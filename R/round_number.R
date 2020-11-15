
#' @title Round Number
#' @description Robust rounding function which is same as round but always rounds up on n.5 no matter the value of n
#' @param x number or numbers to be rounded
#' @param digits number of digits to round to, Default: 0
#' @return rounded number
#' @details R, and other languages do this weird thing where 1.5 is rounded to 2 an so is 2.5. Every .5 is rounded to 
#' the nearest even number. This isn't typically what you want in mathemtics.
#' @rdname round_number
#' @export 

round_number <- function(x, digits = 0) {
  
  if (x - floor(x) == 0.5) {
    
    return(ceiling(x))
    
  } else {
    
    round(x, digits)
    
  }
  
}
