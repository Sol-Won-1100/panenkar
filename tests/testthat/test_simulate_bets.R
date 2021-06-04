
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
  
  bet_sim <- simulate_bets(probs, odds, outcomes, closing_odds, min_advantage = 0.00001)  
 
  # test list structure
  
  expected_output_names <- c("profit_loss", 
                             "roi", 
                             "went_bankrupt",
                             "when_bankrupt",
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
                             "clv_stats_tests",
                             "matches_sampled_for_clv_test")
  
  expect_equivalent(names(bet_sim), expected_output_names) 
  
  expect_equal(bet_sim$profit_loss, 8)
  expect_equal(bet_sim$roi, bet_sim$profit_loss / bet_sim$num_bets)
  
  # Tests for the rolling object
  
  rolling <- bet_sim$rolling
  
  expect_equal(nrow(rolling), length(outcomes) + 1)
  expect_equivalent(rolling$match_num, 0:length(outcomes))
  expect_equivalent(rolling$odds_of_selection, c(NA, 6, NA, 11, 4, 2))
  expect_equivalent(rolling$odds_of_winner, c(NA, 6, 4.1, 1.3, 4, 2))
  
  expect_equivalent(rolling$profit_loss, c(NA, 5, 0, -1, 3, 1))
  expect_equivalent(bet_sim$profit_loss, unlist(rolling[6, "bank_after_match"] - rolling[1, "bank_after_match"]))
 
  rolling$profit_loss[1] <- 100
  
  expect_equivalent(cumsum(rolling$profit_loss), rolling$bank_after_match)
  
  rolling$profit_loss[1] <- NA
  
  expect_equivalent(rolling$outcome[-1], as.character(outcomes))
  expect_equivalent(rolling$bet_result[-1], c("win", "no_bet", "lose", "win", "win"))
  
  expect_equal(bet_sim$num_bets, 4)
  expect_equal(bet_sim$bet_percentage, 0.8)
  expect_equivalent(bet_sim$num_bets_by_outcome, c(1, 1, 2))
  expect_equivalent(bet_sim$bet_percentage_by_outcome, c(1, 1, 2) / length(outcomes))
  expect_equivalent(bet_sim$average_odds_for_bet, mean(rolling$odds_of_selection, na.rm = TRUE))
  expect_equal(bet_sim$num_wins, 3)
  expect_equal(bet_sim$win_percentage, 3 / 4)
  expect_equivalent(bet_sim$average_odds_for_win, mean(c(6, 4, 2)))
  expect_equivalent(class(bet_sim$p_rolling_bank), c("gg", "ggplot"))
  
  expect_equal(bet_sim$clv_advantage, -0.01948734)
    
  # Lets take a scenario where its very simplistic but we have built a model and its unrealistically favorable.
  # We would expect to see an insane ratio smashing all the stats tests out the park

  bet_sim_data <- paste0(load_wd()$tests_testthat, "bet_sim_dummy_data.csv") %>%
    read_csv(col_types = cols(.default = col_double(), outcome = col_character()))
  
  probs <- bet_sim_data %>% select(home_prob:away_prob) %>% as.matrix()
  odds <- bet_sim_data %>% select(home_odds:away_odds) %>% as.matrix()
  closing_odds <- bet_sim_data %>% select(home_closing_odds:away_closing_odds) %>% as.matrix()
  outcomes <- bet_sim_data$outcome %>% factor(levels = c("home", "draw", "away"))
  
  bet_sim <- simulate_bets(probs, odds, outcomes, closing_odds, min_advantage = 0.00001)  

  # Near zero probability that this fails incase it ever falls down!
  
  expect_equivalent(bet_sim$clv_stats_tests$info, rep("Ratio above bounds, evidence of skill", 3))
  
  # Test we dont need to supply closing odds
  
  bet_sim_no_closing <- simulate_bets(probs, odds, outcomes, closing_odds = NA, min_advantage = 0.00001)  
  
  expect_equal(bet_sim_no_closing$clv_advantage, NA)
  expect_equal(bet_sim_no_closing$clv_stats_tests, NA)
  expect_equal(bet_sim_no_closing$matches_sampled_for_clv_test, NA)
  expect_equal(bet_sim_no_closing$profit_loss, bet_sim$profit_loss)
  
  
  # Test we dont need to supply all closing odds
  
  closing_odds_nas <- closing_odds
  closing_odds_nas[1:100,] <- NA
  
  bet_sim_some_closing_na <- simulate_bets(probs, odds, outcomes, closing_odds = closing_odds_nas, 
                                           min_advantage = 0.00001) 
  
  
  expect_equivalent(bet_sim_some_closing_na$rolling$clv_advantage[1:100], rep(NA_real_, 100))
  expect_equivalent(bet_sim_some_closing_na$rolling$bank_after_match[1:100],
                    bet_sim_some_closing_na$rolling$expected_bank_after_match_clv[1:100])
  
  expect_false(isTRUE(all.equal(bet_sim_some_closing_na$rolling$bank_after_match[1:101], 
                                bet_sim_some_closing_na$rolling$expected_bank_after_match_clv[1:101])))
  
  # Test that closing odds NA later dont throw any problems and the first match still starts at position 101
  
  
  closing_odds_nas[110,] <- NA
  
  bet_sim_some_closing_na2 <- simulate_bets(probs, odds, outcomes, closing_odds = closing_odds_nas, 
                                            min_advantage = 0.00001) 
  
  expect_equivalent(bet_sim_some_closing_na$rolling$bank_after_match[1:100],
                    bet_sim_some_closing_na2$rolling$expected_bank_after_match_clv[1:100])
  
  expect_equivalent(bet_sim_some_closing_na2$rolling[, 1:7], bet_sim_some_closing_na$rolling[,1:7])
  
  bet_sim_some_closing_na2$rolling %>%
    filter(match_num == 110) %>%
    select(bet_result, clv_advantage) %>%
    expect_equivalent(tibble(bet_result = "lose", clv_advantage = NA_real_))
  
  
  # Test args can be dfs
  
  bet_sim_probs_df <- simulate_bets(as.data.frame(probs), odds, outcomes, closing_odds, min_advantage = 0.00001)
  bet_sim_odds_df <- simulate_bets(probs, as.data.frame(odds), outcomes, closing_odds, min_advantage = 0.00001)
  bet_sim_closing_df <- simulate_bets(probs, odds, outcomes, as_tibble(as.data.frame(closing_odds)), 
                                      min_advantage = 0.00001)
  
  expect_equivalent(bet_sim_probs_df$rolling, bet_sim$rolling)
  expect_equivalent(bet_sim_odds_df$rolling, bet_sim$rolling)
  expect_equivalent(bet_sim_closing_df$rolling, bet_sim$rolling)
  
  # Test the bankruptcy outputs
  
  bet_sim_bankrupt <- simulate_bets(probs, odds, outcomes, closing_odds, min_advantage = 0.00001, start_bank = 4)  
  
  expect_equal(bet_sim$went_bankrupt, FALSE)
  expect_equal(bet_sim$when_bankrupt, NA_real_)
  
  expect_equal(bet_sim_bankrupt$went_bankrupt, TRUE)
  expect_equal(bet_sim_bankrupt$when_bankrupt, 4)

  # Make sure the stake and start_bank parameters when varied do what we expect
  
  bet_sim_double_stake <- simulate_bets(probs, odds, outcomes, closing_odds, min_advantage = 0.00001, stake = 2)
  
  bet_sim_diff_start_bank <- simulate_bets(probs, odds, outcomes, closing_odds, min_advantage = 0.00001, 
                                                  start_bank = 200)
  
  
  expect_equivalent(bet_sim$profit_loss * 2, bet_sim_double_stake$profit_loss)
  expect_equivalent(bet_sim$profit_loss, bet_sim_diff_start_bank$profit_loss)

  expect_equivalent(
    bet_sim$rolling$bank_after_match - 100, 
    bet_sim_diff_start_bank$rolling$bank_after_match - 200
  )
  
  # And check that odds and wins dont change when you change the stake or start bank
  
  expect_equivalent(bet_sim$num_wins, bet_sim_double_stake$num_wins)
  expect_equivalent(bet_sim$average_odds_for_bet, bet_sim_double_stake$average_odds_for_bet)
  expect_equivalent(bet_sim$average_odds_for_win, bet_sim_double_stake$average_odds_for_win)
  
  expect_equivalent(bet_sim$num_wins, bet_sim_diff_start_bank$num_wins)
  expect_equivalent(bet_sim$average_odds_for_bet, bet_sim_diff_start_bank$average_odds_for_bet)
  expect_equivalent(bet_sim$average_odds_for_win, bet_sim_diff_start_bank$average_odds_for_win)  
  
  # Test out the max odds and advantage arguments, ensuring that they cause fewer bets to be mad
  
  bet_sim_max_odds <- simulate_bets(probs, odds, outcomes, closing_odds, min_advantage = 0.00001, max_odds = 2.5)
  
  expect_lt(bet_sim_max_odds$num_bets, bet_sim$num_bets)

  bet_sim_min_adv <-simulate_bets(probs, odds, outcomes, closing_odds, min_advantage = 0.05)
  
  expect_lt(bet_sim_min_adv$num_bets, bet_sim$num_bets)
  
  # Test that NAs in probs, odds and outcomes doesnt cause problems
  
  probs_na <- probs
  probs_na[3, 1] <- NA
  probs_na[4, 1:2] <- NA
  probs_na[5, 1:3] <- NA
  
  bet_sim_probs_na <- simulate_bets(probs_na, odds, outcomes, closing_odds, min_advantage = 0.00001)
  
  expect_equivalent(bet_sim_probs_na$rolling$bet_result[4:6], rep("no_bet", 3))
  
  odds_na <- odds
  odds_na[3, 1] <- NA
  odds_na[4, 1:2] <- NA
  odds_na[5, 1:3] <- NA
  
  bet_sim_odds_na <- simulate_bets(probs, odds_na, outcomes, closing_odds, min_advantage = 0.00001)
  expect_equivalent(bet_sim_odds_na$rolling$bet_result[4:6], rep("no_bet", 3))

  outcomes_na <- outcomes
  outcomes_na[3:5] <- NA
  
  bet_sim_outcomes_na <- simulate_bets(probs, odds, outcomes_na, closing_odds, min_advantage = 0.00001)
  expect_equivalent(bet_sim_outcomes_na$rolling$bet_result[4:6], rep("no_bet", 3))
  
  
  expect_error(simulate_bets(probs[-1, ], odds, outcomes, closing_odds, min_advantage = 0.00001))
  expect_error(simulate_bets(probs[, -1], odds, outcomes, closing_odds, min_advantage = 0.00001))
  expect_error(simulate_bets(probs, odds[-1,], outcomes, closing_odds, min_advantage = 0.00001))
  expect_error(simulate_bets(probs, odds[,-1], outcomes, closing_odds, min_advantage = 0.00001))
  expect_error(simulate_bets(probs, odds, outcomes[-1], closing_odds, min_advantage = 0.00001))
  expect_error(simulate_bets(probs, odds, outcomes, closing_odds[-1,], min_advantage = 0.00001))
  expect_error(simulate_bets(probs, odds, outcomes, closing_odds[,-1], min_advantage = 0.00001))
  
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

})
