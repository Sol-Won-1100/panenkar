
context("Probit models")

# Setup ----------------------------------------------------------------------------------------------------------------

file_results <- here::here() %>% paste0("/tests/testthat/sco_prem_1994_1995.rds")

results <- read_rds(file_results)
results$match_date <- ymd(results$match_date)

model_data <- probit_build_model_data(results, result, ymd("1995-07-07"))

num_matches <- nrow(results)

fit_probit <- probit_fit(model_data)

# Tests ----------------------------------------------------------------------------------------------------------------

test_that("probit_build_model_data works as expected", {
  
  num_matches <- nrow(results)
  time_weights <- poisson_time_weights(results$match_date, ymd("1995-07-07"))
  
  expect_equal(colnames(model_data), c("match_date", "attack", "defence", "result", "location", "time_weight"))
  expect_equal(num_matches * 2, nrow(model_data))
  
  expect_equal(rep(results$match_date, 2), model_data$match_date)  
  expect_equal(c(results$home_team, results$away_team), model_data$attack)
  expect_equal(c(results$away_team, results$home_team), model_data$defence)
  expect_equal(c(rep(1, num_matches), rep(0, num_matches)), model_data$location)
  expect_equal(rep(time_weights, 2), model_data$time_weight)
  
})

test_that("probit_fit works as expected", {
  
  bad_model_data <- select(model_data, -attack, -result)
  
  coef_names <- fit_probit$coefficients %>% names() %>% sort()
  
  # Aberdeen attack and defence parameters are dropped as they are constrainted
  
  expected_teams <- c(model_data$attack, model_data$defence) %>%
    unique() %>%
    sort() %>%
    magrittr::extract(-1)
  
  expected_paras <- c(paste0("attack", expected_teams), paste0("defence", expected_teams),"draw|loss", "location", 
                      "win|draw")
  
  
  expect_equal(class(fit_probit), "clm")
  expect_equal(coef_names, expected_paras)
  
  expect_error(probit_fit(bad_model_data))
  expect_error(probit_fit("snake"))
  expect_error(probit_fit(model_data, 12))

})

test_that("probit_predict_match works as expected", {
  
  predicted_result <- probit_predict_match("Celtic", "Rangers", fit_probit)

  sum(predicted_result$home_prob, predicted_result$draw_prob, predicted_result$away_prob) %>%
    round() %>%
    expect_equal(1)
  
  expect_equal(nrow(predicted_result), 1)
  
  expect_warning(probit_predict_match("Celtic", "Celtic", fit_probit))
  expect_error(probit_predict_match("Celtic", "Rangers", fit_probit, market = "blah"))
  expect_error(probit_predict_match(c("Celtic", "Hearts"), "Rangers", fit_probit))
  
  barca_output <- tibble(home_prob = NA_real_, draw_prob = NA_real_, away_prob = NA_real_)
  
  expect_equal(probit_predict_match("Celtic", "Barcelona", fit_probit), barca_output)


})


test_that("probit_predict_matches works as expected", {
  
  fixtures <- tibble(home_team = c("Celtic", "Hearts"), away_team = c("Rangers", "Hibernian"))
  
  predicted <- probit_predict_matches(fixtures, fit_probit)
  
  individual_predictions <- bind_rows(probit_predict_match("Celtic", "Rangers", fit_probit),
                                      probit_predict_match("Hearts", "Hibernian", fit_probit))
  
  expect_equal(predicted, individual_predictions)
  expect_error(probit_predict_matches(12, fit_probit))
  
  fixtures_bad1 <- rename(fixtures, goldfish = home_team)
  fixtures_bad2 <- rename(fixtures, pig = away_team)
  
  expect_error(probit_predict_matches(fixtures_bad1, fit_probit))
  
})

test_that("probit_simulate_matches works as expected", {
  
  training_set <- results
  
  test_set <- tibble(match_date = ymd("1995-07-07", "1995-07-11"),
                     home_team = c("Celtic", "Hearts"), 
                     away_team = c("Rangers", "Hibernian"))
  
  predicted <- probit_simulate_matches(training_set , test_set)
  
  individual_predictions <- probit_predict_matches(test_set[1,], fit_probit)
  
  predicted %>%
    slice(1) %>%
    select(-match_date, -home_team, -away_team) %>%
    expect_equal(individual_predictions)
  
})


