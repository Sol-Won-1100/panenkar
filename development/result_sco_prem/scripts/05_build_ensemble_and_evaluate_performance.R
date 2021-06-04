
# TITLE:       Build Ensemble and Evaluate Performance
# DESCRIPTION: Build an ensemble model and evaluate its performance on the result market of the Scottish Premiership

# Setup ----------------------------------------------------------------------------------------------------------------

.competition_id <- "sco_prem"
wd <- load_wd()

# Load model predictions -----------------------------------------------------------------------------------------------

set_model_fitting <- glue("{wd$dev_result_sco_prem_processed_data}set_model_fitting.rds") %>% read_rds()
set_test_main <- glue("{wd$dev_result_sco_prem_processed_data}set_test_main.rds") %>% read_rds()

predicted_poisson_model_fitting <- glue("{wd$dev_result_sco_prem_processed_data}predicted_poisson_model_fitting.rds") %>% 
  read_rds()

predicted_poisson_test_main <- glue("{wd$dev_result_sco_prem_processed_data}predicted_poisson_test_main.rds") %>%
  read_rds()

predicted_probit_model_fitting <- glue("{wd$dev_result_sco_prem_processed_data}predicted_probit_model_fitting.rds") %>%
  read_rds()

predicted_probit_test_main <- glue("{wd$dev_result_sco_prem_processed_data}predicted_probit_test_main.rds") %>% 
  read_rds()

juice_model_fitting <- glue("{wd$dev_result_sco_prem_processed_data}juice_model_fitting.rds") %>% read_rds()
juice_test_main <- glue("{wd$dev_result_sco_prem_processed_data}juice_test_main.rds") %>% read_rds()

min_matches_season <- 10

# Calculate the best ensemble from the model_fitting set ---------------------------------------------------------------

# Matrices better for doing numerical operations

predictions <- list(poisson = predicted_poisson_model_fitting, 
                    probit = predicted_probit_model_fitting, 
                    juice = juice_model_fitting) %>%
  map(~filter(., home_matches_played_season > min_matches_season, away_matches_played_season > min_matches_season)) %>%
  map(~select(., all_of(c("home_prob", "draw_prob", "away_prob")))) %>%
  map(as.matrix)

num_matches <- nrow(predictions$poisson)

observed_model_fitting <- set_model_fitting %>%
  filter(home_matches_played_season > min_matches_season, away_matches_played_season > min_matches_season) %>%
  mutate(home = if_else(result == "home", 1, 0),
         draw = if_else(result == "draw", 1, 0),
         away = if_else(result == "away", 1, 0)) %>%
  select(home, draw, away) %>%
  unlist() %>%
  matrix(nrow = num_matches, ncol = 3)

ensemble_weights <- calc_ensemble_weights(predictions, observed_model_fitting)

# Test ensemble --------------------------------------------------------------------------------------------------------

predictions <- list(poisson = predicted_poisson_test_main, 
                    probit = predicted_probit_test_main, 
                    juice = juice_test_main) %>%
  map(~filter(., home_matches_played_season > min_matches_season, away_matches_played_season > min_matches_season)) %>%
  map(~select(., all_of(c("home_prob", "draw_prob", "away_prob"))))

ensemble_probs <- build_ensemble(predictions, ensemble_weights)

set_test_main_valid_matches <- set_test_main %>% 
  filter(., home_matches_played_season > min_matches_season, away_matches_played_season > min_matches_season)

odds <- select(set_test_main_valid_matches, home_odds_max, draw_odds_max, away_odds_max)
outcomes <- factor(set_test_main_valid_matches$result, c("home", "draw", "away"))#

closing_odds <- select(set_test_main_valid_matches, home_odds_sharp_closing, draw_odds_sharp_closing, 
                       away_odds_sharp_closing)

test_ensemble_0_025 <- simulate_bets(ensemble_probs, odds, outcomes, closing_odds, min_advantage = 0.025)

# Test poisson ---------------------------------------------------------------------------------------------------------

poisson_probs <- select(predictions$poisson, home_prob:away_prob) %>%
  divide_by(1, .) %>%
  remove_margin()

test_poisson_0_025 <- simulate_bets(poisson_probs, odds, outcomes, closing_odds, min_advantage = 0.025)




