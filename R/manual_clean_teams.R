
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
  
  # Raith error
  database %>%
    mutate(
      away_team = case_when(
<<<<<<< HEAD
        away_team == "Raith" ~ "Raith Rvs",
=======
        away_team == "Raith" ~ "Raith Rvs"
>>>>>>> 38ab4f7a5db6e4686e72475350a4e4c7bd5949f1
        TRUE ~ away_team
      ),
      
      match_id = case_when(
<<<<<<< HEAD
        away_team == "Raith" ~ str_replace(match_id, "Raith", "Raith Rvs"),
        TRUE ~ match_id
=======
        away_team == "Raith" ~ str_replace(match_id, "Raith", "Raith Rvs")
        TRUE ~ away_team
>>>>>>> 38ab4f7a5db6e4686e72475350a4e4c7bd5949f1
      )
    )
}

