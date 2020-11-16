
#' @title Round Number
#' @description Robust rounding function which is same as round but always rounds up on n.5 no matter the value of n
#' @param x number or numbers to be rounded
#' @param digits number of digits to round to, Default: 0
#' @return rounded number
#' @details R, and other languages do this weird thing where 1.5 is rounded to 2 an so is 2.5. Every .5 is rounded to 
#' the nearest even number. This isn't typically what you want in mathematics.
#' @rdname round_number
#' @export 

round_number <- function(x, digits = 0) {
  
  is_half <- (x - floor(x)) == 0.5
  is_negative <- x < 0
  
  x_rounded <- x
  
  x_rounded[is_half == FALSE] <- round(x[is_half == FALSE], digits)
  x_rounded[is_half == TRUE & is_negative == FALSE] <- ceiling(x[is_half == TRUE & is_negative == FALSE])  
  x_rounded[is_half == TRUE & is_negative == TRUE] <- floor(x[is_half == TRUE & is_negative == TRUE])  
  
  return(x_rounded)
  
}
