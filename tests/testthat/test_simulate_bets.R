
context("Simulate Bets")

# Create some dummy data to test

num_outcomes <- 2
num_matches <- 100

probs <- runif(num_outcomes * num_matches) %>% 
  matrix(nrow = num_matches, ncol = num_outcomes) %>% 
  divide_by(1, .) %>% 
  remove_margin()

probs_diff <- runif(num_outcomes * num_matches, -0.1, 0.05) %>%
  matrix(nrow = num_matches, ncol = num_outcomes) %>%
  as.data.frame() %>%
  as_tibble()

adjusted_probs <- probs + probs_diff
adjusted_probs[adjusted_probs < 0] <- 0.02

odds <- adjusted_probs %>% divide_by(1, .) %>% round(2)

closing_odds_diff <- runif(num_outcomes * num_matches, -0.025, 0.025) %>%
  matrix(nrow = num_matches, ncol = num_outcomes) %>%
  as.data.frame() %>%
  as_tibble()

closing_odds <- odds %>% divide_by(1, .) %>% add(closing_odds_diff) %>% divide_by(1, .) %>% round(2)

possible_outcomes <- paste0("o", 1:num_outcomes)

outcomes <- possible_outcomes %>% sample(size = num_matches, replace = TRUE) %>% factor(levels = possible_outcomes)

# min_advantage = 0.05
# start_bank = 100
# stake = 1
# max_odds = 5
# tolerance_digits = 3

test_that("simulate_bets list output strucuture is as expected", {

  bet_simulation <- simulate_bets(probs, odds, outcomes, closing_odds)  
  
  expected_output_names <- c("profit_loss", 
                             "roi", 
                             "rolling", 
                             "num_bets", 
                             "bet_percentage", 
                             "num_bets_by_outcome",
                             "bet_percentage_by_outcome",
                             "average_odds_for_bet",
                             "num_wins",
                             "win_percentage",
                             "num_wins_by_outcome",
                             "win_percentage_by_outcome",
                             "average_odds_for_win",
                             "p_rolling_bank",
                             "clv_advantage",
                             "clv_bounds")
  
  expect_equal(names(bet_simulation), expected_output_names)
  expect_equal(class(bet_simulation$p_rolling_bank), c("gg", "ggplot"))
  
})


test_that("simulate_bets summary statistics are correct", {
  
  expect_lt(bet_simulation$profit_loss, num_matches)
  expect_gt(bet_simulation$profit_loss, -num_matches)
  expect_lt(bet_simulation$profit_loss, bet_simulation$num_bets)
  expect_gt(bet_simulation$profit_loss, -bet_simulation$num_bets)
  
  expect_equal(100 * bet_simulation$profit_loss / bet_simulation$num_bets, bet_simulation$roi)
  expect_equal(nrow(bet_simulation$rolling) - 1, num_matches)
  expect_equal(sum(bet_simulation$num_bets_by_outcome), bet_simulation$num_bets)
  expect_equal(sum(bet_simulation$num_wins_by_outcome), bet_simulation$num_wins)
  
  for (i in seq_along(1:num_outcomes)) {
    
    expect_equal(100 * bet_simulation$num_wins_by_outcome[i] / bet_simulation$num_bets_by_outcome[i], 
                 bet_simulation$win_percentage_by_outcome[i])
    
  }
  
})

test_that("simulate_bets error handling works correctly", {
  
  expect_error(simulate_bets(probs, odds, outcomes, closing_odds, min_advantage = c(0.05, 0.05)))
  expect_error(simulate_bets(probs, odds, outcomes, closing_odds, min_advantage = data.frame(c(0.05, 0.05))))
  expect_error(simulate_bets(probs, odds, outcomes, closing_odds, min_advantage = matrix(c(0.05, 0.05))))
  expect_error(simulate_bets(probs, odds, outcomes, closing_odds, min_advantage = "cat"))
  expect_error(simulate_bets(probs, odds, outcomes, closing_odds, min_advantage = numeric(0)))
  expect_error(simulate_bets(probs, odds, outcomes, closing_odds, min_advantage = NULL))
  
  expect_warning(simulate_bets(probs, odds, outcomes, closing_odds, min_advantage = -runif(1)))
  expect_warning(simulate_bets(probs, odds, outcomes, closing_odds, min_advantage = 0))
  
  expect_error(simulate_bets(probs, odds, outcomes, closing_odds, start_bank = c(100, 100)))
  expect_error(simulate_bets(probs, odds, outcomes, closing_odds, start_bank = data.frame(c(100, 100))))
  expect_error(simulate_bets(probs, odds, outcomes, closing_odds, start_bank = matrix(c(100, 100))))
  expect_error(simulate_bets(probs, odds, outcomes, closing_odds, start_bank = "cat"))
  expect_error(simulate_bets(probs, odds, outcomes, closing_odds, start_bank = numeric(0)))
  expect_error(simulate_bets(probs, odds, outcomes, closing_odds, start_bank = NULL))
  
  expect_warning(simulate_bets(probs, odds, outcomes, closing_odds, start_bank = 0))
  expect_warning(simulate_bets(probs, odds, outcomes, closing_odds, start_bank = -20))
  
  # Finish testing all the variables and add in tests for the shape of probs / odds and tests for their contents etc  
  
})

