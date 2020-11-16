
context("Poisson models")

# Setup ----------------------------------------------------------------------------------------------------------------

file_results <- here::here() %>% paste0("/tests/testthat/sco_prem_1994_1995.rds")

results <- read_rds(file_results)
results$match_date <- ymd(results$match_date)

model_data <- poisson_build_model_data(results, home_goals, away_goals, ymd("1995-07-07"))

fit_poisson <- poisson_fit(model_data)
fit_zero_inflated <- poisson_fit(model_data, TRUE)

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
  
  expect_error(poisson_fit(bad_model_data))
  expect_error(poisson_fit("snake"))
  expect_error(poisson_fit(model_data, 12))
  
    
})


test_that("poisson_predict_match with standard poisson model works as expected", {
  
  predicted_result <- poisson_predict_match("Celtic", "Rangers", fit_poisson)
  predicted_over_under <- poisson_predict_match("Celtic", "Rangers", fit_poisson, markets = "over_under")
  predicted_btts <- poisson_predict_match("Celtic", "Rangers", fit_poisson, markets = "both_teams_to_score")
  
  predicted_combined <- predicted_result %>%
    left_join(predicted_over_under) %>%
    left_join(predicted_btts)

  predicted_all <- poisson_predict_match("Celtic", "Rangers", fit_poisson, 
                                         markets = c("result", "over_under", "both_teams_to_score"))
  
  expect_equal(predicted_all, predicted_combined)
  
  sum(predicted_all$home_prob, predicted_all$draw_prob, predicted_all$away_prob) %>%
    round() %>%
    expect_equal(1)
  
  sum(predicted_all$over_2.5_prob, predicted_all$under_2.5_prob) %>%
    round() %>%
    expect_equal(1)
  
  sum(predicted_all$btts_yes_prob, predicted_all$btts_no_prob) %>%
    round() %>%
    expect_equal(1) 
  
  expect_gt(predicted_all$home_prob, predicted_all$away_prob)
  expect_equal(nrow(predicted_all), 1)
  
  expect_warning(poisson_predict_match("Celtic", "Celtic", fit_poisson))
  
  expect_error(poisson_predict_match(c("Celtic", "Aberdeen"), c("Rangers", "Hearts"), fit_poisson))
  expect_error(poisson_predict_match("Celtic", "Rangers", fit_poisson, markets = "blah"))
  expect_error(poisson_predict_match("Celtic", "Rangers", fit_poisson, markets = 2222))
  expect_error(poisson_predict_match("Celtic", "Rangers", fit_poisson, max_goals = -1))
  expect_error(poisson_predict_match("Celtic", "Rangers", fit_poisson, max_goals = 0))
  expect_error(poisson_predict_match("Celtic", "Rangers", fit_poisson, max_goals = 7.3))
  expect_error(poisson_predict_match("Celtic", "Rangers", fit_poisson, max_goals = "dog"))
  expect_error(poisson_predict_match("Celtic", "Rangers", fit_poisson, zero_inflated = 12))
  expect_error(poisson_predict_match("Celtic", "Rangers", fit_poisson, markets = "over_under", 
                                     over_under_goals = "dog"))
  
  expect_error(poisson_predict_match("Celtic", "Rangers", fit_poisson, markets = "over_under", over_under_goals = -2))
  expect_error(poisson_predict_match("Celtic", "Rangers", fit_poisson, markets = "result", over_under_goals = "cat"), 
               NA)
  
  expect_error(poisson_predict_match("Celtic", "Rangers", fit_poisson, markets = "result", over_under_goals = -2), NA)

})

test_that("poisson_predict_match with zero inflated model works as expected", {
  
  predicted_result <- poisson_predict_match("Celtic", "Rangers", fit_zero_inflated)
  predicted_over_under <- poisson_predict_match("Celtic", "Rangers", fit_zero_inflated, markets = "over_under")
  predicted_btts <- poisson_predict_match("Celtic", "Rangers", fit_zero_inflated, markets = "both_teams_to_score")
  
  predicted_combined <- predicted_result %>%
    left_join(predicted_over_under) %>%
    left_join(predicted_btts)
  
  predicted_all <- poisson_predict_match("Celtic", "Rangers", fit_zero_inflated, 
                                         markets = c("result", "over_under", "both_teams_to_score"))
  
  expect_equal(predicted_all, predicted_combined)
  
  sum(predicted_all$home_prob, predicted_all$draw_prob, predicted_all$away_prob) %>%
    round() %>%
    expect_equal(1)
  
  sum(predicted_all$over_2.5_prob, predicted_all$under_2.5_prob) %>%
    round() %>%
    expect_equal(1)
  
  sum(predicted_all$btts_yes_prob, predicted_all$btts_no_prob) %>%
    round() %>%
    expect_equal(1) 
  
  expect_gt(predicted_all$home_prob, predicted_all$away_prob)
  expect_equal(nrow(predicted_all), 1)
  
  expect_warning(poisson_predict_match("Celtic", "Celtic", fit_poisson))
  expect_error(poisson_predict_match("Celtic", "Rangers", fit_poisson, markets = "blah"))
  expect_error(poisson_predict_match("Celtic", "Rangers", fit_poisson, markets = 2222))
  expect_error(poisson_predict_match("Celtic", "Rangers", fit_poisson, max_goals = -1))
  expect_error(poisson_predict_match("Celtic", "Rangers", fit_poisson, max_goals = 0))
  expect_error(poisson_predict_match("Celtic", "Rangers", fit_poisson, max_goals = 7.3))
  expect_error(poisson_predict_match("Celtic", "Rangers", fit_poisson, max_goals = "dog"))
  expect_error(poisson_predict_match("Celtic", "Rangers", fit_poisson, zero_inflated = 12))
  expect_error(poisson_predict_match("Celtic", "Rangers", fit_poisson, markets = "over_under", 
                                     over_under_goals = "dog"))
  
  expect_error(poisson_predict_match("Celtic", "Rangers", fit_poisson, markets = "over_under", over_under_goals = -2))
  expect_error(poisson_predict_match("Celtic", "Rangers", fit_poisson, markets = "result", over_under_goals = "cat"), 
               NA)
  
  expect_error(poisson_predict_match("Celtic", "Rangers", fit_poisson, markets = "result", over_under_goals = -2), NA)
  
})

test_that("poisson_predict_matches works as expected", {
  
  fixtures <- tibble(home_team = c("Celtic", "Hearts"), away_team = c("Rangers", "Hibernian"))
  
  predicted <- poisson_predict_matches(fixtures, fit_poisson)
  
  individual_predictions <- bind_rows(poisson_predict_match("Celtic", "Rangers", fit_poisson),
                                      poisson_predict_match("Hearts", "Hibernian", fit_poisson))
  
  expect_equal(predicted, individual_predictions)
  expect_error(poisson_predict_matches(12, fit_zero_inflated))
  
  fixtures_bad1 <- rename(fixtures, goldfish = home_team)
  fixtures_bad2 <- rename(fixtures, pig = away_team)
  
  expect_error(poisson_predict_matches(fixtures_bad1, fit_poisson))
  expect_error(poisson_predict_matches(fixtures_bad2, fit_zero_inflated))
  
})

test_that("poisson_simulate_matches works as expected", {
  
  training_set <- results
  
  test_set <- tibble(match_date = ymd("1995-07-07", "1995-07-11"),
                     home_team = c("Celtic", "Hearts"), 
                     away_team = c("Rangers", "Hibernian"))
  
  predicted <- poisson_simulate_matches(training_set , test_set)
  
  individual_predictions <- poisson_predict_matches(test_set[1,], fit_poisson)
  
  predicted %>%
    slice(1) %>%
    select(-match_date, -home_team, -away_team) %>%
    expect_equal(individual_predictions)
  
  expect_error(poisson_simulate_matches(training_set, test_set, zero_inflated = "wombat"))
    
})

