
#' Remove Margin
#'
#' Given a bookmakers implied probability remove the bookmakers margin
#'
#' @param prob_with_margin the implied probability of a selection occurring. 
#' This is equal to 1 / bookmakers odds.
#' @param margin the bookmakers margin, this is equal to the sum of the implied
#' probability of all selections minus 1.
#' @param num_selections the number of selections, for the full time market 
#' this would be 3
#' @param method by default this is set to the 'proportional' method detailed
#' here: https://www.football-data.co.uk/The_Wisdom_of_the_Crowd_updated.pdf but
#' can also be the 'straight' method. The proportional method is thought to be
#' superior, straight method is just divide everything by the overround.
#' 
#' @return The probability with the margin removed.
#' @examples
#' odds_home <- 2.5
#' odds_draw <- 3
#' odds_away <- 3.5
#' 
#' margin <- 1 / odds_home + 1 / odds_draw + 1/ odds_away
#' probs_margin_removed <- remove_margin(1/odds_home, margin)

remove_margin <- function(prob_with_margin, margin, num_selections = 3, 
                          method = "proportional"){
  
  if(method == "straight"){
    prob_no_margin <- prob_with_margin / (1 + margin)
  } else if(method == "proportional"){
    prob_no_margin <- (prob_with_margin * num_selections - margin) / num_selections
    
  } else {
    stop("bad 'method' supplied must be one of c('straight', 'proportional')")
  }
  
}