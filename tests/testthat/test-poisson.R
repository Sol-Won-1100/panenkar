
context("Poisson models")

# Setup ----------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(lubridate)

# Delete when complete
library(panenkar)
library(testthat)

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
  expect_error(poisson_time_weights(date1, date2, ))
  
})




test_that("poisson_build_model_data works as expected", {

  num_matches <- nrow(results)
  
  expect_equal(colnames(model_data), c("match_date", "attack", "defence", "goals", "location", "time_weight"))
  expect_equal(num_matches * 2, nrow(model_data))
  
  expect_equal(rep(results$match_date, 2), model_data$match_date)  
  expect_equal(c(results$home_team, results$away_team), model_data$attack)
  expect_equal(c(results$away_team, results$home_team), model_data$defence)
  expect_equal(c(results$home_goals, results$away_goals), model_data$goals)
  expect_equal(c(rep(1, num_matches), rep(0, num_matches)), model_data$location)
  
})


test_that("error handling works as expected for poisson_predict_match", {
  
  
})