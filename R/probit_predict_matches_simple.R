
#' Probit Predict Matches Simple
#' 
#' Predict multiple matches using probit model with no updating of training sets or anything
#' 
#' @param fixtures match fixtures, columns of which must be "home_team" and "away_team". For the rest of the parameters
#'        and return see probit_predict_match.R

probit_predict_matches_simple <- function (fixtures, fit, market = "result") {
  
  
  fit_vars <- names(fit$xlevels)
  
  predictions <- fixtures %>%
    select(all_of(fit_vars)) %>%
    map(~unlist(.)) %>%
    pmap_dfr(probit_predict_match, fit, market)

  return(predictions)
  
}
