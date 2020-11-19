
context("Goal Difference Model")

# Setup ----------------------------------------------------------------------------------------------------------------

file_results <- here::here() %>% paste0("/tests/testthat/sco_prem_1994_1995.rds")

results <- readr::read_rds(file_results)
results$match_date <- lubridate::ymd(results$match_date)

Tests ----------------------------------------------------------------------------------------------------------------

  
# Do we really need gd_calc_ratings???
  
testthat::test_that("gd_calc_rating and gd_calc_ratings work as expected", {

  match_rating1 <- panenkar::gd_calc_rating(results, "Celtic",  ymd("1995-07-07"), xi = 0.0016)
  match_rating2 <- gd_calc_rating(results, "Celtic",  ymd("1995-07-07"), xi = 0.0016, max_gd = 20)
  
  testthat::expect_equal(is.numeric(match_rating1), TRUE)
  expect_equal(length(match_rating1), 1)
  expect_equal(match_rating1, match_rating2)
  
  expect_equal(gd_calc_rating(results, "Cardiff",  ymd("1995-07-07"), xi = 0.0016), NA_real_)

  testthat::expect_error(gd_calc_rating(results, c("Celtic", "Hearts"),  ymd("1995-07-07"), xi = 0.0016))
  expect_error(gd_calc_rating(tibble("blah"), "Celtic",  ymd("1995-07-07"), xi = 0.0016))
  expect_error(gd_calc_rating(12, "Celtic",  ymd("1995-07-07"), xi = 0.0016))
  expect_error(gd_calc_rating(results, "Celtic", rep(ymd("1995-07-07"), 2), xi = 0.0016))
  expect_error(gd_calc_rating(results, "Celtic", ymd("1995-07-07"), xi = c(1,1)))
  
  # And the gd_calc_ratings (multiple teams) one too
  
  results %>%
    gd_calc_ratings(c("Celtic", "Hearts"),  ymd("1995-07-07"), xi = 0.0016) %>% 
    expect_equal(c(match_rating1, match_rating2))
  
  expect_error(gd_calc_ratings(results, 1:4,  ymd("1995-07-07"), xi = 0.0016))

})


test_that("gd_add_ratings works as expected", {
  
  results_with_ratings <- gd_add_ratings(results, xi = 0.0016)
  
  away_team <- results_with_ratings %>% dplyr::slice(n()) %>% dplyr::select(away_team) %>% unlist()
  
  final_date <- results_with_ratings %>% 
    slice(n()) %>% 
    select(match_date) %>% 
    dplyr::mutate(match_date = as.character(match_date)) %>%
    unlist() %>%
    lubridate::ymd()
  
  rating1 <- results_with_ratings %>%
    slice(n()) %>%
    select(away_rating) %>%
    unlist()
  
  rating2 <- results %>% 
    slice(1:(n() - 1)) %>% 
    gd_calc_ratings(teams = away_team, current_date = final_date, xi = 0.0016)
  
  # Different names in the vectors of the output so just extract the values only.
  
  x1 <- c()
  x1[1] <- rating1[1]
  
  x2 <- c()
  x2[1] <- rating2[1]
  
  expect_equal(x1, x2)
  
  expect_equal(nrow(results), nrow(results_with_ratings))
  
  cols_expected <- c(colnames(results), "home_rating", "away_rating", "match_rating")
  
  expect_equal(colnames(results_with_ratings), cols_expected)
    
  
})


historic_results <- results


  
  
  
  if (market == "result") {
    
    
    
    expected_cols <- c("match_rating", "result")
    
    check_arg_results(historic_results, expected_cols)
    
    fit_result <- historic_results %>%
      mutate(result = factor(result, levels = c("home", "draw", "away"))) %>%
      ordinal::clm(result ~ match_rating, data = .)
    
  }
  
  






test_that("gd_predict_matches works as expected", {
  
  
  historic_results <- gd_add_ratings(historic_results, xi, max_gd, min_matches)
  
  
  gd_predict_matches(historic_results, fit, match_rating, market = "result", over_under_goals = 2.5)
  
})


test_that("gd_simulate_matches works as expected", {
  
  
})



test_that("gd_optim_xi and .gd_optim_xi work as expected", {
  
  
})




