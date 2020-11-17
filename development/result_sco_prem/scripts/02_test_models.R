
# TITLE:       Modelling Full Time Result Market for Scottish Premiership
# DESCRIPTION: Analysis of betting strategy for this market

# Setup ----------------------------------------------------------------------------------------------------------------

library(panenkar)

wd <- list()

competition_id <- "sco_prem"
subproject_path <- "development/results_sco_prem/"

# Set paths

wd <- list()

wd$wd <- glue("{here::here()}/")

wd$subproject <- glue("{wd$wd}{subproject_path}")
wd$raw_data <- glue("{wd$subproject}raw_data/")
wd$processed_data <- glue("{wd$subproject}processed_data/")
wd$results <- glue("{wd$subproject}results/")

# Get data and split into sets -----------------------------------------------------------------------------------------

results <- "{wd$raw_data}results_main_{competition_id}.rds" %>%
  glue() %>% 
  read_rds() %>% 
  mutate(match_date = ymd(match_date))

# The first set is for initializing models. 
# The second set is for fitting models to and combining or what have you.
# The third set is for testing the models for profitability.
# The fourth set is for testing performance against the closing line. This is a subset to the third set because we dont
# have as much closing line data.

season_ids_initial_training <- sequence_seasons("1994_1995", "2000_2001")
season_ids_model_fitting <-  sequence_seasons("2000_2001", "2006_2007")
season_ids_test_main <- sequence_seasons("2007_2008", "2019_2020")
# season_ids_test_closing <- sequence_seasons() # add in later

select_match_ids <- function (results, season_ids) {
  
  results %>% filter(season_id %in% season_ids) %>% select(match_id) %>% unlist()
  
}

match_ids_initial_training <- select_match_ids(results, season_ids_initial_training)
match_ids_model_fitting <- select_match_ids(results, season_ids_model_fitting)
match_ids_test_main <- select_match_ids(results, season_ids_test_main)
# match_ids_test_closing <- select_match_ids(results, season_ids_test_closing) - add in later

set_inital_training <- filter(results, match_id %in% match_ids_initial_training)
set_model_fitting <- filter(results, match_id %in% match_ids_model_fitting)
set_test_main <- filter(results, match_id %in% match_ids_test_main_training)


## Fit models ----------------------------------------------------------------------------------------------------------

## Poisson model

predicted_poisson_model_fitting <- poisson_simulate_matches(set_inital_training, set_model_fitting, 
                                                            markets = "result")

predicted_poisson_test_main <- initial_training_set %>% 
  bind_rows(model_fitting_set) %>%
  poisson_simulate_matches(model_test_main_set, markets = "result")

predicted_poisson_model_fitting %>% write_rds(file = glue("{wd$processed_data}predicted_poisson_model_fitting.rds"))
predicted_poisson_test_main %>% write_rids(file = glue("{wd$processed_data}predicted_poisson_test_main.rds"))


## Probit model

glue("{wd$processed_data}predicted_probit.rds") %>% write_rds(predicted_probit, file = .)


predicted_probit_model_fitting <- probit_simulate_matches(set_inital_training, set_model_fitting)

predicted_probit_test_main <- initial_training_set %>% 
  bind_rows(model_fitting_set) %>%
  probit_simulate_matches(model_test_main_set)

predicted_probit_model_fitting %>% write_rds(file = glue("{wd$processed_data}predicted_probit_model_fitting.rds"))
predicted_probit_test_main %>% write_rids(file = glue("{wd$processed_data}predicted_probit_test_main.rds"))


## Goal difference model





# Predict using gd rating model ----------------------------------------------------------------------------------------

# First we need to optimize the parameters for this model

optim_train_season_ids_gd <- sequence_seasons("1994_1995", "1999_2000")

optim_train_match_ids_gd <- results %>% 
  filter(season_id %in% optim_train_season_ids_gd) %>% 
  select(match_id) %>% 
  unlist()

max_gds <- c(2:6, NA_real_)

optim_xi_output <- list()

gd_training_set <- filter(results, match_id %in% optim_train_match_ids_gd)
gd_test_set <- filter(results, match_id %in% test_set2_match_ids)

max_gd <- 2

for(max_gd in max_gds) {
  
  optim_xi_output[[i]] <- gd_optim_xi(gd_training_set, gd_test_set, start_xi = 0.0016, max_gd, min_matches = 10)
    
}








predicted_gd <- gd_simulate_matches(training_set, test_set, xi_optim, max_gd_optim)

wd$output %>% paste0("predicted_gd.rds") %>% write_rds(predicted_gd, file = .)

predicted_gd <- wd$output %>% paste0("predicted_gd.rds") %>% read_rds()


## BELOW HERE NEEDS TO GO TO NEW SCript


# Ensemble optimized against actual outcomes ---------------------------------------------------------------------------

min_matches_season <- 10

# Matrices better for doing numerical operations

predictions <- list(poisson = predicted_poisson, zero_inflated = predicted_zero_inflated, probit = predicted_probit) %>%
  map(~filter(., match_id %in% test_set1_match_ids)) %>%
  map(add_matches_played) %>%
  map(~filter(., home_matches_played_season > min_matches_season, away_matches_played_season > min_matches_season)) %>%
  map(~select(., all_of(c("home_prob", "draw_prob", "away_prob"))))

num_matches <- nrow(predictions$poisson)

predictions <- map(predictions, ~matrix(unlist(.), nrow = num_matches, ncol = 3))

observed <- test_set %>%
  filter(match_id %in% test_set1_match_ids) %>%
  add_match_numbers_season() %>%
  filter(home_match_number_season > min_matches_season, away_match_number_season > min_matches_season) %>%
  mutate(home = if_else(result == "home", 1, 0),
         draw = if_else(result == "draw", 1, 0),
         away = if_else(result == "away", 1, 0)) %>%
    select(home, draw, away) %>%
  unlist() %>%
  matrix(nrow = num_matches, ncol = 3)

ensemble_weights <- calc_ensemble_weights(predictions, observed)

ensemble <- build_ensemble(predictions, ensemble_weights)









