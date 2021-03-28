
#' @title Calculate Advantage
#' @description Calculate the percentage advantage for a given bet 
#' @param probs true / estimated probabilities
#' @param odds odds on offer at the bookmaker
#' @return the advantage as a percentage
#' @details Advantage is equal to expected profit
#' @examples 
#'  modelled_probs <- 0.5
#'  bookmakers_odds <- 2.1
#' 
#' calc_advantage(modelled_probs, bookmakers_odds)
#' 
#' # Alternatively you could measure your tipped odds against closing odds
#' 
#' pinnacle_closing_odds <- 2.05
#' 
#' calc_advantage(1 / bookmakers_odds, pinnacle_closing_odds)
#' 
#' @rdname calc_advantage
#' @export 

calc_advantage <- function(probs, odds) {
  
  probs * odds - 1
  
}
