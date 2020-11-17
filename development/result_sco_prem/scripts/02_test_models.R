
# TITLE:       Modelling Full Time Result Market for Scottish Premiership
# DESCRIPTION: Analysis of betting strategy for this market

# Setup ----------------------------------------------------------------------------------------------------------------

library(panenkar)

wd <- list()

competition_id <- "sco_prem"

wd$wd <- here::here() %>% paste0("/development/full-time-result/", competition_id, "/")
wd$data <- paste0(wd$wd, "data/")
wd$output <- paste0(wd$wd, "output/")


# Get data and assign sets ---------------------------------------------------------------------------------------------

results <- wd$data %>% paste0("results-main-sco_prem.rds") %>% read_rds() %>% mutate(match_date = ymd(match_date))

training_set <- filter(results, season_id == min_season(season_id))
test_set <- filter(results, season_id != min_season(season_id))

# Test set 1 will be used for optimizing the ensemble models and adding in the juice. his will be performed on seasons 
# 0001 --> 0607.

# Test set 2 will be used for measuring accuracy and profitability out of sample. THis takes places from 0708 --> 1920.

test_season_ids1 <- sequence_seasons("2000_2001", "2006_2007")
test_season_ids2 <- sequence_seasons("2007_2008", "2019_2020")

test_set1_match_ids <- test_set %>% filter(season_id %in% test_season_ids1) %>% select(match_id) %>% unlist()
test_set2_match_ids <- filter(test_set, season_id %in% test_season_ids2) %>% select(match_id) %>% unlist()

min_matches_season <- 10


# Predict using poisson model ------------------------------------------------------------------------------------------

## Commented out, already ran and outputted, quicker to output and read back in
#
# predicted_poisson <- poisson_simulate_matches(training_set, test_set, markets = "result")
# 
# wd$output %>% paste0("predicted_poisson.rds") %>% write_rds(predicted_poisson, file = .)

predicted_poisson <- wd$output %>% paste0("predicted_poisson.rds") %>% read_rds()


# Predict using zero inflated poisson model ----------------------------------------------------------------------------

#
# predicted_zero_inflated <- poisson_simulate_matches(training_set, test_set, markets = "result", zero_inflated = TRUE,
#                                                     weight_cut_off = 0.02)
# 
# wd$output %>% paste0("predicted_zero_inflated.rds") %>% write_rds(predicted_zero_inflated, file = .)

predicted_zero_inflated <- wd$output %>% paste0("predicted_zero_inflated.rds") %>% read_rds()


# Predict using basic probit model -------------------------------------------------------------------------------------

# predicted_probit <- probit_simulate_matches(training_set, test_set)
# 
# wd$output %>% paste0("predicted_probit.rds") %>% write_rds(predicted_probit, file = .)


predicted_probit <- wd$output %>% paste0("predicted_probit.rds") %>% read_rds()


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



# Ensemble optimized against actual outcomes ---------------------------------------------------------------------------

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









