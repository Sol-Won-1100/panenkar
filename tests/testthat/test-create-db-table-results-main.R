
context("Create Database Table: Results Main")

# Setup ----------------------------------------------------------------------------------------------------------------

wd_test <- here::here() %>% paste0("/tests/testthat/")

file_raw_main <- paste0(wd_test, "tur_sl_2019_2020.csv") 
results_main_league_raw <- read_csv(file_raw_main)
results_main_league <- create_db_table_results_main_fd_main(file_raw_main)

file_raw_extra <- paste0(wd_test, "den_sl_all.csv")
results_extra_league_raw <- read_csv(file_raw_extra)
results_extra_league <- create_db_table_results_main_fd_extra(file_raw_extra)

# Tests ----------------------------------------------------------------------------------------------------------------

test_that("columns and rows are as expected", {
  
  # Test for main leagues
  
  expected_cols <- c("match_id", "competition_id", "season_id", "match_date", "home_team", "away_team", "result",
                     "home_goals", "away_goals", "over_under_2_5", "total_goals", "home_odds_max", "draw_odds_max",
                     "away_odds_max", "over_odds_max", "under_odds_max", "home_odds_sharp_closing", 
                     "draw_odds_sharp_closing", "away_odds_sharp_closing", "over_odds_sharp_closing", 
                     "under_odds_sharp_closing","is_valid_result", "is_location_home", "is_replay", "leg", 
                     "is_empty_stadium")
  
  num_valid_rows_results <- results_main_league_raw %>% filter(!is.na(Date)) %>% nrow()
  
  expect_equivalent(colnames(results_main_league), expected_cols)
  expect_equal(nrow(results_main_league), num_valid_rows_results)
  
  # Tests for extra leagues, expected cols same
  
  num_valid_rows_results <- results_extra_league_raw %>% filter(!is.na(Date)) %>% nrow()

  expect_equivalent(colnames(results_extra_league), expected_cols)
  expect_equal(nrow(results_extra_league), num_valid_rows_results)  
  
  
})

test_that("max odds calculated correctly", {
  
  max_home_odds_expected <- c(4.5, 2.1, 2.46, 2.5)
  max_draw_odds_expected <- c(3.8, 3.4, 3.52, 3)
  max_away_odds_expected <- c(1.83, 3.6, 3.02, 5)
  max_over_odds_expected <- c(1.72, 2.05, 2.09, 2)
  max_under_odds_expected <- c(2.07, 1.75, 1.86, 2.2)

  expect_equal(max_home_odds_expected, results_main_league$home_odds_max)
  expect_equal(max_draw_odds_expected, results_main_league$draw_odds_max)
  expect_equal(max_away_odds_expected, results_main_league$away_odds_max)
  expect_equal(max_over_odds_expected, results_main_league$over_odds_max)
  expect_equal(max_under_odds_expected, results_main_league$under_odds_max)
  
  max_home_odds_expected <- c(2, 2.65, 2.4)
  max_draw_odds_expected <- c(3.4, 3.35, 4.2)
  max_away_odds_expected <- c(3.6, 3.25, 3.33)
  max_over_odds_expected <- c(2, 2.5, 3)
  max_under_odds_expected <- rep(NA_real_, 3)
  
  expect_equal(max_home_odds_expected, results_extra_league$home_odds_max)
  expect_equal(max_draw_odds_expected, results_extra_league$draw_odds_max)
  expect_equal(max_away_odds_expected, results_extra_league$away_odds_max)
  expect_equal(max_over_odds_expected, results_extra_league$over_odds_max)
  expect_equal(max_under_odds_expected, results_extra_league$under_odds_max)

})


test_that("closing odds picked up ok", {

  home_odds_sharp_closing_expected <- c(4.15, 2.11, 3.55, 2.66)
  draw_odds_sharp_closing_expected <- c(3.75, 3.46, 3.37, 3.61)
  away_odds_sharp_closing_expected <- c(1.9, 3.66, 2.19, 2.62)
  over_odds_sharp_closing_expected <- c(1.76, 2.06, 2.05, 1.73)
  under_odds_sharp_closing_expected <- c(2.13, 1.82, 1.83, 2.19)
  
  expect_equal(home_odds_sharp_closing_expected, results_main_league$home_odds_sharp_closing)
  expect_equal(draw_odds_sharp_closing_expected , results_main_league$draw_odds_sharp_closing)
  expect_equal(away_odds_sharp_closing_expected, results_main_league$away_odds_sharp_closing)
  expect_equal(over_odds_sharp_closing_expected, results_main_league$over_odds_sharp_closing)
  expect_equal(under_odds_sharp_closing_expected, results_main_league$under_odds_sharp_closing)
  
  home_odds_sharp_closing_expected <- c(4.15, 2.11, 3.55)
  draw_odds_sharp_closing_expected <- rep(NA_real_, 3)
  away_odds_sharp_closing_expected <- c(1.9, 3.66, 2.19)
  over_odds_sharp_closing_expected <- c(2.1, NA_real_, 2)
  under_odds_sharp_closing_expected <- rep(NA_real_, 3)
  
  expect_equal(home_odds_sharp_closing_expected, results_extra_league$home_odds_sharp_closing)
  expect_equal(draw_odds_sharp_closing_expected , results_extra_league$draw_odds_sharp_closing)
  expect_equal(away_odds_sharp_closing_expected, results_extra_league$away_odds_sharp_closing)
  expect_equal(over_odds_sharp_closing_expected, results_extra_league$over_odds_sharp_closing)
  expect_equal(under_odds_sharp_closing_expected, results_extra_league$under_odds_sharp_closing)
  
})

test_that("error returned if file does not exist", {
  
  expect_error(create_db_table_results_main_fd_main("path-that-doesnt-exist.csv"))
  expect_error(create_db_table_results_extra_fd_main("path-that-doesnt-exist.csv"))
  
})

test_that("error returned if bad file name", {
  
  file_bad_season_id <- paste0(wd_test, "tur_sl_2019__2020.csv") 
  expect_equal(file.exists(file_bad_season_id ), TRUE)
  
  expect_error(create_db_table_results_main_fd_main(file_bad_season_id)) # No season_id pattern in file name
  expect_error(create_db_table_results_main_fd_extra(file_bad_season_id)) # No _all in file name
  
})

test_that("error returned if wrong file type", {
  
  file_bad_type <- paste0(wd_test, "tur_sl_2019_2020.xlsx") 
  expect_equal(file.exists(file_bad_type), TRUE)
  expect_error(create_db_table_results_main_fd_main(file_bad_type)) 
  
  file_bad_type <- paste0(wd_test, "den_sl_all.xlsx") 
  expect_equal(file.exists(file_bad_type), TRUE)
  expect_error(create_db_table_results_main_fd_extra(file_bad_season_id)) # No _all in file name
  
})

test_that("calc_max_odds helper works as expected", {
  
  x <- tribble(~home_odds_coral,  ~home_odds_william_hill, ~away_odds_coral,
               4.5,               4.2,                     NA_real_,
               NA_real_,          2,                       NA_real_)
  
  expect_equal(calc_max_odds(x, "home_odds"), c(4.5, 2))
  expect_equal(calc_max_odds(x, "draw_odds"), rep(NA_real_, 2))
  expect_equal(calc_max_odds(x, "away_odds"), rep(NA_real_, 2))
})



