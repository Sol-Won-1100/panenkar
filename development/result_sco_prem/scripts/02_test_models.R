
# TITLE:       Modelling Full Time Result Market for Scottish Premiership
# DESCRIPTION: Analysis of betting strategy for this market

# Setup ----------------------------------------------------------------------------------------------------------------

library(panenkar)

wd <- list()

competition_id <- "sco_prem"
subproject_path <- "development/result_sco_prem/"

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

season_ids_initial_training <- sequence_seasons("1994_1995", "1999_2000")
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
set_test_main <- filter(results, match_id %in% match_ids_test_main)


## Fit models ----------------------------------------------------------------------------------------------------------

## Poisson model ----

predicted_poisson_model_fitting <- poisson_simulate_matches(set_inital_training, set_model_fitting, 
                                                            markets = "result")

predicted_poisson_test_main <- set_inital_training %>% 
  bind_rows(set_model_fitting) %>%
  poisson_simulate_matches(set_test_main, markets = "result")

predicted_poisson_model_fitting %>% write_rds(file = glue("{wd$processed_data}predicted_poisson_model_fitting.rds"))
predicted_poisson_test_main %>% write_rds(file = glue("{wd$processed_data}predicted_poisson_test_main.rds"))


## Probit model ----

predicted_probit_model_fitting <- probit_simulate_matches(set_inital_training, set_model_fitting)

predicted_probit_test_main <- set_inital_training %>% 
  bind_rows(set_model_fitting) %>%
  probit_simulate_matches(set_test_main)

predicted_probit_model_fitting %>% write_rds(file = glue("{wd$processed_data}predicted_probit_model_fitting.rds"))
predicted_probit_test_main %>% write_rds(file = glue("{wd$processed_data}predicted_probit_test_main.rds"))


## Goal difference model ----

# We firstly want to optimise xi. For speed we will do this only on the initial_training_set taking the first season
# for initializing our model. We will leave max_gd = NA and optimize this separately once we have a good value for xi.

set_optim_xi_initial <- filter(set_inital_training, season_id == min(season_id))
set_optim_xi_test <- filter(set_inital_training, season_id != min(season_id))

# Step 1 - optimize xi

optim_output <- gd_optim_xi(set_optim_xi_initial, set_optim_xi_test, max_gd = NA, min_matches = 10)

optim_output %>% write_rds(file = glue("{wd$results}gd_optim_xi_output.rds"))


# Add code to extract xi from optim_output

xi <- optim_output$minimum

max_gds <- 2:8
store_rps <- list()

# Calc rps for various trimmed goal differences - this should smooth out massive variance if a team collapses and gets
# rolled over

for (i in seq_along(1:length(max_gds))) {
  
  store_rps[[i]] <- .gd_optim_xi(xi, set_optim_xi_initial, set_optim_xi_test, max_gd = max_gds[i], min_matches = 10)
  
}


names(store_rps) <- max_gds

store_rps %>% write_rds(file = glue("{wd$results}gd_rps_for_various_max_gd.rds"))

df <- tibble(max_gd = c(max_gds, "no_restriction"), rps = c(unlist(store_rps), optim_output$objective)) 

p <- df %>%
  ggplot(aes(x = max_gd, y = rps, group = 1)) +
  geom_line() +
  ylim(min(df$rps) * 0.99, max(df$rps) * 1.01)

p # So no restrictions seems the way forward

xi_optim <- xi

# Predict

predicted_gd_model_fitting <- gd_simulate_matches(set_inital_training, set_model_fitting, xi = xi_optim,
                                                  max_gd = NA, market = "result")

predicted_gd_test_main <- set_inital_training %>% 
  bind_rows(set_model_fitting) %>% 
  gd_simulate_matches(set_test_main, xi = xi_optim, max_gd = NA, market = "result")

predicted_gd_model_fitting %>% write_rds(file = glue("{wd$processed_data}predicted_gd_model_fitting.rds"))
predicted_gd_test_main %>% write_rds(file = glue("{wd$processed_data}predicted_gd_test_main.rds"))




## BELOW HERE NEEDS TO GO TO NEW SCript


# Ensemble optimized against actual outcomes ---------------------------------------------------------------------------

min_matches_season <- 10

# Matrices better for doing numerical operations

predictions <- list(poisson = predicted_poisson_model_fitting, 
                    probit = predicted_probit_model_fitting, 
                    gd = predicted_gd_model_fitting) %>%
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









