
context("Poisson models")

# Tests the following functions from stable-poisson.R
#
# poisson_time_weights
# poisson_build_model_data
# poisson_fit
#
# To Do
# poisson_simulate_matches
# poisson_predict_matches
# poisson_predict_match



# Setup ----------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(lubridate)

# # Delete when complete
# library(panenkar)
# library(testthat)

file_results <- here::here() %>% paste0("/tests/testthat/sco_prem_1994_1995.rds")

results <- read_rds(file_results)
results$match_date <- ymd(results$match_date)

model_data <- poisson_build_model_data(results, home_goals, away_goals, ymd("1995-07-07"))



# Tests ----------------------------------------------------------------------------------------------------------------

test_that("poisson time weights works as expected", {
  
  date1 <- ymd("2020-01-01")
  date2 <- ymd("2018-06-07")

  various_dates <- date1 + round(runif(10, -300, 300))
  
   
  expect_equal(poisson_time_weights(date1, date2),0) # Current date >= match date returns 0
  expect_equal(poisson_time_weights(date1, date1),0) 
  expect_gt(poisson_time_weights(date2, date1), 0) 
  expect_equal(length(poisson_time_weights(various_dates, date1)), 10)
    
  
  expect_error(poisson_time_weights(12, date1))
  expect_error(poisson_time_weights(date2, "cat"))
  expect_warning(poisson_time_weights(date1, date2, xi = -0.0015))
  expect_error(poisson_time_weights(date1, date2, xi = "X"))
  
})


test_that("poisson_build_model_data works as expected", {

  num_matches <- nrow(results)
  time_weights <- poisson_time_weights(results$match_date, ymd("1995-07-07"))
  
  
  expect_equal(colnames(model_data), c("match_date", "attack", "defence", "goals", "location", "time_weight"))
  expect_equal(num_matches * 2, nrow(model_data))
  
  expect_equal(rep(results$match_date, 2), model_data$match_date)  
  expect_equal(c(results$home_team, results$away_team), model_data$attack)
  expect_equal(c(results$away_team, results$home_team), model_data$defence)
  expect_equal(c(results$home_goals, results$away_goals), model_data$goals)
  expect_equal(c(rep(1, num_matches), rep(0, num_matches)), model_data$location)
  expect_equal(rep(time_weights, 2), model_data$time_weight)
  
})


test_that("poisson_fit works as expected", {
  
  fit_poisson <- poisson_fit(model_data)
  fit_zero_inflated <- poisson_fit(model_data, TRUE)
  bad_model_data <- select(model_data, -attack, -goals)
  
  
  coef_names_poisson <- fit_poisson$coefficients %>% names() %>% sort()
  coef_names_zero_inflated <- fit_zero_inflated$coefficients$count %>% names() %>% sort()
  
  # Aberdeen attack and defence parameters are dropped as they are constrainted
  
  expected_teams <- c(model_data$attack, model_data$defence) %>%
    unique() %>%
    sort() %>%
    magrittr::extract(-1)
  
  expected_paras <- c("(Intercept)", paste0("attack", expected_teams), paste0("defence", expected_teams), "location")
  
  
  expect_equal(class(fit_poisson), c("glm", "lm"))
  expect_equal(class(fit_zero_inflated), "zeroinfl")
  expect_equal(coef_names_poisson, coef_names_zero_inflated)
  expect_equal(coef_names_poisson, expected_paras)
  
  expect_error(fit_poisson(bad_model_data))
  expect_error(fit_poisson("snake"))
  expect_error(fit_poisson(model_data, 12))
  
    
})


test_that("", {
  

  
})


