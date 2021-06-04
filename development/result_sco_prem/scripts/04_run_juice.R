
# TITLE: Run Juice
# DESCRIPTION: A model based on market probabilities

# Setup ----------------------------------------------------------------------------------------------------------------

.competition_id <- "sco_prem"
wd <- load_wd()

set_inital_training <- read_rds(glue("{wd$dev_result_sco_prem_processed_data}set_inital_training.rds"))
set_model_fitting <- read_rds(glue("{wd$dev_result_sco_prem_processed_data}set_model_fitting.rds"))
set_test_main <- read_rds(glue("{wd$dev_result_sco_prem_processed_data}set_test_main.rds"))

## Run model -----------------------------------------------------------------------------------------------------------

juice_model_fitting <- set_model_fitting %>%
  select(home_odds_max:away_odds_max) %>%
  remove_margin() %>%
  set_colnames(c("home_prob", "draw_prob", "away_prob")) %>%
  bind_cols(select(set_model_fitting, match_id, season_id, home_team, away_team), .)

juice_test_main <- set_test_main %>%
  select(home_odds_max:away_odds_max) %>%
  remove_margin() %>%
  set_colnames(c("home_prob", "draw_prob", "away_prob")) %>%
  bind_cols(select(set_test_main, match_id, season_id, home_team, away_team), .)

write_rds(juice_model_fitting, file = glue("{wd$dev_result_sco_prem_processed_data}juice_model_fitting.rds"))
write_rds(juice_test_main, file = glue("{wd$dev_result_sco_prem_processed_data}juice_test_main.rds"))