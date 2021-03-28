
# TITLE:       Build Ensemble and Evaluate Performance
# DESCRIPTION: Build an ensemble model and evaluate its performance on the result market of the Scottish Premiership

# Setup ----------------------------------------------------------------------------------------------------------------

library(panenkar)

wd <- list()

competition_id <- "sco_prem"
subproject_path <- "development/result_sco_prem/"

wd$subproject <- glue("{wd$wd}{subproject_path}")
wd$processed_data <- glue("{wd$subproject}processed_data/")





# Calculate ensemble weights -------------------------------------------------------------------------------------------

set_model_fitting <- glue("{wd$processed_data}set_model_fitting.rds") %>% read_rds()
set_test_main <- glue("{wd$processed_data}set_test_main.rds") %>% read_rds()

predicted_poisson_model_fitting <- glue("{wd$processed_data}predicted_poisson_model_fitting.rds") %>% read_rds()
predicted_poisson_test_main <- glue("{wd$processed_data}predicted_poisson_test_main.rds") %>% read_rds()
predicted_probit_model_fitting <- glue("{wd$processed_data}predicted_probit_model_fitting.rds") %>% read_rds()
predicted_probit_test_main <- glue("{wd$processed_data}predicted_probit_test_main.rds") %>% read_rds()
predicted_gd_model_fitting <- glue("{wd$processed_data}predicted_gd_model_fitting.rds") %>% read_rds()
predicted_gd_test_main <- glue("{wd$processed_data}predicted_gd_test_main.rds") %>% read_rds()
juice_model_fitting <- glue("{wd$processed_data}juice_model_fitting.rds") %>% read_rds()
juice_test_main <- glue("{wd$processed_data}juice_test_main.rds") %>% read_rds()

min_matches_season <- 10

# Matrices better for doing numerical operations

predictions <- list(poisson = predicted_poisson_model_fitting, 
                    probit = predicted_probit_model_fitting, 
                    gd = predicted_gd_model_fitting,
                    juice = juice_model_fitting) %>%
  map(add_matches_played) %>%
  map(~filter(., home_matches_played_season > min_matches_season, away_matches_played_season > min_matches_season)) %>%
  map(~select(., all_of(c("home_prob", "draw_prob", "away_prob"))))

num_matches <- nrow(predictions$poisson)

predictions <- map(predictions, ~matrix(unlist(.), nrow = num_matches, ncol = 3))

observed_model_fitting <- set_model_fitting %>%
  add_matches_played() %>%
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
                    gd = predicted_gd_test_main,
                    juice = juice_test_main) %>%
  map(add_matches_played) %>%
  map(~filter(., home_matches_played_season > min_matches_season, away_matches_played_season > min_matches_season)) %>%
  map(~select(., all_of(c("home_prob", "draw_prob", "away_prob"))))

ensemble <- build_ensemble(predictions, ensemble_weights)

set_test_main_valid_matches <- set_test_main %>% 
  add_matches_played() %>%
  filter(., home_matches_played_season > min_matches_season, away_matches_played_season > min_matches_season)

odds <- set_test_main_valid_matches %>%
  select(home_odds_max, draw_odds_max, away_odds_max) %>%
  as.matrix() 


model_probs <- ensemble
bookmakers_odds <- odds
outcomes <- factor(set_test_main_valid_matches$result, c("home", "draw", "away"))
min_advantage <- 0.1


# AKA expected return on investment








# Test v closing odds --------------------------------------------------------------------------------------------------

# Basically calculate the rps and mse for implied probs against my probs? No I think what I should do is get the odds
# when I elected to bet and see how profitable my predictions would have been then.

















