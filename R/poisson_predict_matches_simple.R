
#' Poisson Predict Matches Simple
#' 
#' Predict multiple matches using Poisson regression with no updating of training sets or anything
#' 
#' @param fixtures match fixtures, columns of which must be "home_team" and "away_team". For the rest of the parameters
#'        and return see poisson_predict_match.R

poisson_predict_matches_simple <- function (fixtures, fit, max_goals = 8, market = "result", over_under_goals = 2.5) {
  
  fit_vars <- names(fit$xlevels)
  
  predictions <- fixtures %>%
    select(all_of(fit_vars)) %>%
    map(~unlist(.)) %>%
    pmap_dfr(probit_predict_match, fit, market, max_goals, market, over_under_goals)
  
  return(predictions)
  
}

