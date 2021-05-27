
context("Simulate Bets")

test_that("simulate_bets works as expected", {

  sim_inputs1 <- create_dummy_sim_data(num_matches = 30, num_outcomes = 2, .seed = 12)
  bet_simulation <- simulate_bets(sim_inputs1$probs, sim_inputs1$odds, sim_inputs1$outcomes, sim_inputs1$closing_odds)  
  
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
  
  expect_equivalent(names(bet_simulation), expected_output_names)
  
  expect_equivalent(class(bet_simulation$p_rolling_bank), c("gg", "ggplot"))
  
  expect_lt(bet_simulation$profit_loss, num_matches)
  expect_gt(bet_simulation$profit_loss, -num_matches)
  expect_gt(bet_simulation$profit_loss, -bet_simulation$num_bets)
  
  expect_equivalent(100 * bet_simulation$profit_loss / bet_simulation$num_bets, bet_simulation$roi)
  expect_equivalent(nrow(bet_simulation$rolling) - 1, num_matches)
  expect_equivalent(sum(bet_simulation$num_bets_by_outcome), bet_simulation$num_bets)
  expect_equivalent(sum(bet_simulation$num_wins_by_outcome), bet_simulation$num_wins)
  
  for (i in seq_along(1:num_outcomes)) {
    
    expect_equal(100 * bet_simulation$num_wins_by_outcome[i] / bet_simulation$num_bets_by_outcome[i], 
                 bet_simulation$win_percentage_by_outcome[i])
    
  }
  
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
