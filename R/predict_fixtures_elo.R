
#' @title Predict Fixtures ELO
#'
#' @description Uses logistic regression and historical elo ratings to predict
#' fair probabilities and odds for matches
#'
#' @param .competition_id see get_metadata for format of competition ids
#' @param results_elo standard results tibble but with columns for home_elo_pre
#' and away_elo_pre
#' 
#' @export

predict_fixtures_elo <- function(.competition_id, results_elo, elo_latest){
  
  results_elo_competition <- filter(results_elo, 
                                    competition_id == .competition_id)
  
  # Fit the model
  
  first_season_id <- unlist(results_elo_competition[1, "season_id"])
  
  df <- results_elo_competition %>%
    dplyr::filter(season_id != first_season_id) %>%
    dplyr::select(match_result, elo_home_pre, elo_away_pre) %>%
    mutate(match_result = factor(match_result, c("home", "draw", "away")))
  
  fit <- polr(match_result ~ elo_home_pre + elo_away_pre, df)
  
  # Create fixtures list with all combinations of possible fixtures. In the future
  # could be smarter and obtain the actual fixtures.
  
  elo_latest_competition <- elo_latest[[.competition_id]]
  
  fixtures <- list(home_team = elo_latest_competition$team, 
                   away_team = elo_latest_competition$team) %>%
    cross_df() %>%
    filter(home_team != away_team) %>%
    left_join(elo_latest_competition, by = c("home_team" = "team")) %>%
    left_join(elo_latest_competition, by = c("away_team" = "team")) %>%
    magrittr::set_colnames(c("home_team", "away_team", "elo_home_pre", 
                             "elo_away_pre"))
  
  predicted_only_probs <- fit %>%
    predict(newdata = fixtures, type = "probs") %>%
    as_tibble() %>%
    magrittr::set_colnames(c("home_prob", "draw_prob", "away_prob")) %>%
    bind_cols(fixtures, .) %>%
    mutate(home_odds = round(1 / home_prob, 2),
           draw_odds = round(1 / draw_prob, 2),
           away_odds = round(1 / away_prob, 2),
           competition_id = .competition_id) 
  
  return(predicted_only_probs)
}
