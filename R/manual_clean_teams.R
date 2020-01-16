
#' @title Manual Clean Teams
#'
#' @description Manually correct some errors in football data. See team-notes.R
#' for details of errors
#'
#' @param database either database_results or database_odds
#'
#' @export
#'
#' @examples
#' manual_clean_teams(database_results)

manual_clean_teams <- function(database){
  
  database # %>%
    # mutate(away_team = if_else(away_team == "Raith", "Raith Rvs", away_team),
    #       match_id = str_replace(match_id, "Raith", "Raith Rvs"))
}

