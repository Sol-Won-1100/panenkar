
#' @title Update ELO
#'
#' @description Update ELO rating
#'
#' @param elo_home The current elo rating of the home team
#' @param elo_away The current elo rating of the away team
#' @param home home, draw and away take one of the following formats. Either an
#'             indicator to show what the actual result was e.g. home = 1, 
#'             draw = 0, away = 0 for a home win or the probability of each 
#'             outcome derived via bookmakers odds or some other method. 
#' for more info on different types of league at www.football-data.co.uk.
#' @param draw see above
#' @param away see above
#' 
#' @note The functions default values are configured for the 'odds' methods 
#'       where probabilities derived via bookmakers odds are used. It has been
#'       shown that this method is superior for generating an ELO rating than
#'       standard methods of using outcomes only.
#'       
#'
#' @examples
#' odds <- c(2.5,3.2,4)
#' probs <- 1 / odds
#' probs <- probs / sum(probs)
#' elo_home <- 800
#' elo_away <- 950
#'
#' update_elo(800, 950, probs[1], probs[2], probs[3], 
#'            output_format = "named_vector")

update_elo <- function(elo_home, elo_away, home, draw, away, 
                       advantage_home = 80, x = 10, y = 400, z = 175, 
                       output_format = "tibble"){
  
  if(round(sum(home, draw, away),4) != 1){
    stop("sum of actuals must = 1")
  }
  
  expected_home <- 1 / (1 + x ^ ((elo_away - elo_home - advantage_home) / y))
  expected_away <- 1 - expected_home
  
  home <- home + draw * 0.5
  away <- away + draw * 0.5
  
  elo_home_new <- elo_home + z * (home - expected_home)
  elo_away_new <- elo_away + z * (away - expected_away)
  
  if(output_format == "tibble"){
    elos_new <- tibble(elo_home = elo_home_new, elo_away = elo_away_new)
    
  } else if(output_format == "named_vector"){
    elos_new <- c(elo_home = elo_home_new, elo_away = elo_away_new)
    
  } else {
    elos_new <- c(elo_home_new, elo_away_new)
  }
  
  return(elos_new)
}
