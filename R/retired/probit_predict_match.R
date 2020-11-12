
#' Probit Predict Match
#' 
#' Predict a match using ordered probit model
#' 
#' @param home_team the name of the home_team
#' @param away_team the name of the away_team
#' @param fit fitted model using ordered probit
#' @param market only "result" applicable currently
#'        
#' @return a tibble of probabilities for each outcome


probit_predict_match <- function(home_team, away_team, fit, market = "result"){
  
  home_team <- unlist(home_team)
  away_team <- unlist(away_team)
 
  teams_attack <- fit$xlevels$attack
  teams_defence <- fit$xlevels$defence
   
  if (!(home_team %in% teams_attack) | !(home_team %in% teams_defence) | !(away_team %in% teams_attack) |
      !(away_team %in% teams_defence)) {
    
    if (market == "result") {
      
      return(tibble(home_prob = NA_real_,  draw_prob = NA_real_, away_prob = NA_real_))
      
    } else {
      
      stop("bad market supplied")
      
    }
    
  }
  
  # Construct prediction data

  probs <- tibble(location = 1, attack = home_team, defence = away_team) %>%
    predict(fit, ., type = "prob") %>% 
    unlist()

  if (market == "result") {

    return(tibble(home_prob = probs[1], draw_prob = probs[2], away_prob = probs[3]))
    
  } else {
    
    stop("bad market supplied")
    
  }
  
}
  
