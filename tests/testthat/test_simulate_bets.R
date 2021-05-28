
context("Simulate Bets")

test_that("create_dummy_sim_data works as expected", {
  
  # Structure is fine
  
  expect_equivalent(create_dummy_sim_data(1, 2, .seed = 112), create_dummy_sim_data(1, 2, .seed = 112))
  expect_equivalent(names(create_dummy_sim_data(12, 13)), 
                    c("estimated_probs", "bookmakers_odds", "outcomes", "bookmakers_closing_odds"))

  sim_inputs <- create_dummy_sim_data(100000, 3)

  probs <- sim_inputs$estimated_probs
  odds <- sim_inputs$bookmakers_odds
  closing_odds <- sim_inputs$bookmakers_odds
  
  expect_equivalent(length(probs[probs > 1 | probs < 0]), 0)
  expect_equivalent(length(odds[odds < 1]), 0)
  expect_equivalent(length(closing_odds[closing_odds < 1]), 0)
  expect_equivalent(is_same_size(probs, odds, closing_odds), TRUE)
      
})


test_that("simulate_bets works as expected", {

  probs <- tribble(~V1,  ~V2,  ~V3,
                             0.5,  0.3,  0.2,
                             0.6, 0.24, 0.16,
                             0.8,  0.1,  0.1,
                             0.55, 0.27, 0.18,
                             0.52, 0.23, 0.25)
  
  probs <- as.matrix(probs)
  
  odds <- tribble(~V1,  ~V2,  ~V3,
                       1.9, 3, 6,
                          1.6,  4.1,  6.2,
                          1.3,    6,   11,
                          1.7,    4,  5.5,
                            2,  3.8,    4)

  odds <- as.matrix(odds)
  
  closing_odds <- tribble(~V1,  ~V2,  ~V3,
                       1.9, 3,       5.8,
                       1.6,  4.1,    6.2,
                       1.3,    6,     10,
                       1.7,  4.1,    5.5,
                       1.9,  3.8,      4
                    )

  closing_odds <- as.matrix(closing_odds)
  
  outcomes <- factor(c("away", "draw", "home", "draw", "home"), levels = c("home", "draw", "away"))
  
  
  bet_simulation <- simulate_bets(probs, odds, outcomes, closing_odds, min_advantage = 0)  
  
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
