

context("Build Bet Placement Matrix")

test_that("build_bet_placement_matrix works as expected", {

  probs <- tribble(~home, ~draw, ~away,
                   0.5,   0.3,     0.2,
                   0.35,  0.3,     0.35,
                   0.5,   0.25,     0.25)
  
  odds <- tribble(~home, ~draw, ~away,
                  2.1,   3.2,   4.8,
                  2.9,   3,     2.7,
                  2,     4.1,   4.2)
  
  min_advantage <- 0.02
  max_odds <- NA
  
  bet_placement_matrix <- matrix(0, nrow = nrow(probs), ncol = ncol(probs))
  bet_placement_matrix[1, 1] <- 1
  bet_placement_matrix[3, 3] <- 1
  
  expect_equal(build_bet_placement_matrix(probs, odds, min_advantage, max_odds), bet_placement_matrix)

  expect_error(build_bet_placement_matrix(select(probs, -home), odds, min_advantage, max_odds))
  expect_error(build_bet_placement_matrix(probs, select(odds, -home), min_advantage, max_odds))
  expect_error(build_bet_placement_matrix(probs, odds, min_advantage = NA, max_odds))

  expect_error(build_bet_placement_matrix(probs, odds, c(min_advantage, min_advantage), max_odds))
  expect_error(build_bet_placement_matrix(probs, odds, min_advantage, c(max_odds, max_odds))) 
  expect_error(build_bet_placement_matrix(probs, odds, "cat", max_odds)) 
  expect_error(build_bet_placement_matrix(probs, odds, min_advantage, "dog")) 

})
