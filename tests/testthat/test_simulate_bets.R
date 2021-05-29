
context("Simulate Bets")

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
  
  bet_simulation <- simulate_bets(probs, odds, outcomes, closing_odds, min_advantage = 0.00001)  
  
  # test list structure
  
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
                             "clv_bounds",
                             "matches_sampled_for_clv_test")
  
  expect_equivalent(names(bet_simulation), expected_output_names) 
  
  expect_equal(bet_simulation$profit_loss, 8)
  expect_equal(bet_simulation$roi, bet_simulation$profit_loss / bet_simulation$num_bets)
  
  # Tests for the rolling object
  
  rolling <- bet_simulation$rolling
  
  expect_equal(nrow(rolling), length(outcomes) + 1)
  expect_equivalent(rolling$match_num, 0:length(outcomes))
  expect_equivalent(rolling$odds_of_selection, c(NA, 6, NA, 11, 4, 2))
  expect_equivalent(rolling$odds_of_winner, c(NA, 6, 4.1, 1.3, 4, 2))
  
  expect_equivalent(rolling$profit_loss, c(NA, 5, 0, -1, 3, 1))
  expect_equivalent(bet_simulation$profit_loss, unlist(rolling[6, "bank_after_match"] - rolling[1, "bank_after_match"]))
 
  rolling$profit_loss[1] <- 100
  
  expect_equivalent(cumsum(rolling$profit_loss), rolling$bank_after_match)
  
  rolling$profit_loss[1] <- NA
  
  expect_equivalent(rolling$outcome[-1], as.character(outcomes))
  expect_equivalent(rolling$bet_result[-1], c("win", "no_bet", "lose", "win", "win"))
  
  expect_equal(bet_simulation$num_bets, 4)
  expect_equal(bet_simulation$bet_percentage, 0.8)
  expect_equivalent(bet_simulation$num_bets_by_outcome, c(1, 1, 2))
  expect_equivalent(bet_simulation$bet_percentage_by_outcome, c(1, 1, 2) / length(outcomes))
  expect_equivalent(bet_simulation$average_odds_for_bet, mean(rolling$odds_of_selection, na.rm = TRUE))
  expect_equal(bet_simulation$num_wins, 3)
  expect_equal(bet_simulation$win_percentage, 3 / 4)
  expect_equivalent(bet_simulation$average_odds_for_win, mean(c(6, 4, 2)))
  expect_equivalent(class(bet_simulation$p_rolling_bank), c("gg", "ggplot"))
  
  expect_equal(bet_simulation$clv_advantage, -0.01948734)
    
  
  # Lets take a scenario where its very simplistic but we have built a model and its unrealistically favorable.
  # We would expect to see an insane ratio smashing all the stats tests out the park

  probs <- bet_sim_data %>% select(home_prob:away_prob) %>% as.matrix()
  odds <- bet_sim_data %>% select(home_odds:away_odds) %>% as.matrix()
  closing_odds <- bet_sim_data %>% select(home_closing_odds:away_closing_odds) %>% as.matrix()
  outcomes <- bet_sim_data$outcome %>% factor(levels = c("home", "draw", "away"))
  
  bet_simulation <- simulate_bets(probs, odds, outcomes, closing_odds, min_advantage = 0.00001)  
  
  # Near zero probability that this fails incase it ever falls down!
  
  expect_equivalent(bet_simulation$clv_bound$info, rep("Ratio above bounds, evidence of skill", 3))
  
  # Test args can be dfs
  
  bet_sim_probs_df <- simulate_bets(as.data.frame(probs), odds, outcomes, closing_odds, min_advantage = 0.00001)
  bet_sim_odds_df <- simulate_bets(probs, as.data.frame(odds), outcomes, closing_odds, min_advantage = 0.00001)
  bet_sim_closing_df <- simulate_bets(probs, odds, outcomes, as_tibble(as.data.frame(closing_odds)), 
                                      min_advantage = 0.00001)
  
  expect_equivalent(bet_sim_probs_df$rolling, bet_simulation$rolling)
  expect_equivalent(bet_sim_odds_df$rolling, bet_simulation$rolling)
  expect_equivalent(bet_sim_closing_df$rolling, bet_simulation$rolling)
  
  # Tests to write
  # See below for good single arg tests
  # Test if stake is double then original output pl * 2 = new sim pl
  # Similar tests for start_bank changing then pl constant but final bank x higher
  # Test if max odds set then num bets goes down
  # Same if min advantage increased
  # Different size probs, odds, closing_odds creates problems
  
  # TO Do
  
  # Remove the .seed arg, not needed
  # Write code to be able to handle when closing odds not supplied
  # Test code when closing odds starts part way through
  # Try putting some NAs into probs, odds, closing odds and make sure those matches are skipped but everything else 
  # plays out OK
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Now test the arguments, error handling and expected behaviour
  
  expect_error(simulate_bets(cbind(probs, probs[,1]), odds, outcomes, closing_odds, min_advantage = 0.00001))
  expect_error(simulate_bets(as.data.frame(probs), odds, outcomes, closing_odds, min_advantage = 0.00001))
  
  
  
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
