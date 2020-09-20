
#' Probit Predict Matches Simple
#' 
#' Predict multiple matches using probit model with no updating of training sets or anything
#' 
#' @param fixtures match fixtures, columns of which must be "home_team" and "away_team". For the rest of the parameters
#'        and return see probit_predict_match.R

probit_predict_matches_simple <- function (fixtures, fit, market = "result") {
  
  list(home_team = fixtures$home_team, away_team = fixtures$away_team) %>%
    pmap_dfr(probit_predict_match, fit,  market)
  
}
