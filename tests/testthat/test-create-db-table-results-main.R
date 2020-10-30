
# To Do - create draw and away test and over under and closing

context("Create Database Table: Results Main")

library(tidyverse)
library(lubridate)
library(magrittr)

# Delete these 2 once complete
library(panenkar)
library(testthat)

# Setup ----------------------------------------------------------------------------------------------------------------

wd_test_data <- here::here() %>% paste0("/tests/testthat/")
file_raw <- paste0(wd_test_data, "tur_sl_2019_2020.csv") 

results_raw <- read_csv(file_raw)
results_main <- create_db_table_results_main_fd_main(file_raw)

# Tests ----------------------------------------------------------------------------------------------------------------

test_that("columns and rows are as expected", {
  
  expected_cols <- c("match_id", "competition_id", "season_id", "match_date", "home_team", "away_team", "result",
                     "home_goals", "away_goals", "over_under_2_5", "total_goals", "home_odds_max", "draw_odds_max",
                     "away_odds_max", "over_odds_max", "under_odds_max", "home_odds_sharp_closing", 
                     "draw_odds_sharp_closing", "away_odds_sharp_closing", "over_odds_sharp_closing", 
                     "under_odds_sharp_closing","is_valid_result", "is_location_home", "is_replay", "leg", 
                     "is_empty_stadium")
  
  num_valid_rows_results <- results_raw %>% filter(!is.na(Date)) %>% nrow()
  
  expect_equivalent(colnames(results_main), expected_cols)
  expect_equal(nrow(results_main), num_valid_rows_results)
  
})

test_that("max odds calculated correctly", {
  
  max_home_odds_expected <- c(4.5, 2.1, 2.46, 2.5)
  max_draw_odds_expected <- c(3.8, 3.4, 3.52, 3)
  max_away_odds_expected <- c(1.83, 3.6, 3.02, 5)
  max_over_odds_expected <- c(1.72, 2.05, 2.09, 2)
  max_under_odds_expected <- c(2.07, 1.75, 1.86, 2.2)

  expect_equivalent(max_home_odds_expected, results_main$home_odds_max)
  expect_equivalent(max_draw_odds_expected, results_main$draw_odds_max)
  expect_equivalent(max_away_odds_expected, results_main$away_odds_max)
  expect_equivalent(max_over_odds_expected, results_main$over_odds_max)
  expect_equivalent(max_under_odds_expected, results_main$under_odds_max)
  
})


test_that("closing odds picked up ok", {

  home_odds_sharp_closing_expected <- c(4.15, 2.11, 3.55, 2.66)
  draw_odds_sharp_closing_expected <- c(3.75, 3.46, 3.37, 3.61)
  away_odds_sharp_closing_expected <- c(1.9, 3.66, 2.19, 2.62)
  over_odds_sharp_closing_expected <- c(1.76, 2.06, 2.05, 1.73)
  under_odds_sharp_closing_expected <- c(2.13, 1.82, 1.83, 2.19)
  
  expect_equivalent(home_odds_sharp_closing_expected, results_main$home_odds_sharp_closing)
  expect_equivalent(draw_odds_sharp_closing_expected , results_main$draw_odds_sharp_closing)
  expect_equivalent(away_odds_sharp_closing_expected, results_main$away_odds_sharp_closing)
  expect_equivalent(over_odds_sharp_closing_expected, results_main$over_odds_sharp_closing)
  expect_equivalent(under_odds_sharp_closing_expected, results_main$under_odds_sharp_closing)

})

test_that("error returned if file does not exist", {
  
  expect_error(create_db_table_results_main_fd_main("path-that-doesnt-exist.csv"))
  
})

test_that("error returned if bad file name", {
  
  file_bad_season_id <- paste0(wd_test_data, "tur_sl_2019__2020.csv") 
  expect_error(create_db_table_results_main_fd_main(file_bad_season_id))
  
})



test_that("add_missing_col_name helper works as expected", {
  
  x <- tribble(~x1,  ~x2, ~x3,
               "dog", 2,   4)
  
  y <- mutate(x, x4 = NA_real_)
  
  expect_equal(add_missing_col_name(x, "x1"), x)
  expect_equal(add_missing_col_name(x, "x4"), y)
  
})


test_that("calc_max_odds helper works as expected", {
  
  x <- tribble(~home_odds_coral,  ~home_odds_william_hill, ~away_odds_coral,
               4.5,               4.2,                     4.8,
               NA_real_,          2,                       2.2)

  expect_equal(calc_max_odds(x, "home_odds"), c(4.5, 2))

})



