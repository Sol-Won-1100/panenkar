
#' @title Calculate Time Weights Exponential
#'
#' @description Uses an exponetial time weighting for modelling, weighting more
#' recent matches more strongly. This is derived from the time weighting method
#' used by Dixon-Coles in their version of the double Poisson model
#'
#' @param match_dates dates of football matches, date object
#' @param current_date what date to calculate the exponential time weight from
#' @param xi the time weight parameter
#'       
#' @export

calc_time_weights_exp <- function(match_dates, current_date, xi = 0.0016){
  
  days_difference <- as.numeric(current_date - match_dates)
  
  weights <- exp(-xi*days_difference)
  weights[days_difference <= 0] <- 0
  
  return(weights)
  
}