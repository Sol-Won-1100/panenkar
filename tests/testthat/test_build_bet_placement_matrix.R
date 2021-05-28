

context("Build Bet Placement Matrix")

test_that("build_bet_placement_matrix works as expected", {

  probs1 <- tribble(~home, ~draw, ~away,
                   0.5,   0.3,     0.2,
                   0.35,  0.3,     0.35,
                   0.5,   0.25,     0.25)
  
  odds1 <- tribble(~home, ~draw, ~away,
                  2.1,   3.2,   4.8,
                  2.9,   3,     2.7,
                  2,     4.1,   4.2)
  
  probs2 <- tribble(~V1,  ~V2,  ~V3,
                   0.5,  0.3,  0.2,
                   0.6, 0.24, 0.16,
                   0.8,  0.1,  0.1,
                   0.55, 0.27, 0.18,
                   0.52, 0.23, 0.25)
  
  odds2 <- tribble(~V1,  ~V2,  ~V3,
                  1.9,   3,    6,
                  1.6,  4.1,  6.2,
                  1.3,    6,   11,
                  1.7,    4,  5.5,
                  2,  3.8,    4)

  bet_placement_matrix1 <- matrix(0, nrow = nrow(probs1), ncol = ncol(probs1))
  
  expect_equivalent(build_bet_placement_matrix(probs1, odds1, 0, min(odds1) - 0.01), bet_placement_matrix1)
  
  bet_placement_matrix1[1, 1] <- 1
  bet_placement_matrix1[2, 1] <- 1
  bet_placement_matrix1[3, 3] <- 1
  
  expect_equivalent(build_bet_placement_matrix(probs1, odds1, 0, NA), bet_placement_matrix1)
  
  bet_placement_matrix2 <- matrix(0, nrow = nrow(probs2), ncol = ncol(probs2))
  
  expect_equivalent(build_bet_placement_matrix(probs2, odds2, 0.21, NA), bet_placement_matrix2)
  
  bet_placement_matrix2[1, 3] <- 1
  bet_placement_matrix2[3, 3] <- 1
  bet_placement_matrix2[4, 2] <- 1
  bet_placement_matrix2[5, 1] <- 1
  
  expect_equivalent(build_bet_placement_matrix(probs2, odds2, 0, NA), bet_placement_matrix2)
  
  odds2[,1] <- NA
  
  expect_equivalent(build_bet_placement_matrix(probs2, odds2, 0, NA), 
                    matrix(0, nrow = nrow(probs2), ncol = ncol(probs2)))
  
  odds2[,1] <- Inf
  
  expect_equivalent(build_bet_placement_matrix(probs2, odds2, 0, NA), 
                    matrix(0, nrow = nrow(probs2), ncol = ncol(probs2)))
  
  odds2[,1] <- -Inf
  
  expect_equivalent(build_bet_placement_matrix(probs2, odds2, 0, NA), 
                    matrix(0, nrow = nrow(probs2), ncol = ncol(probs2)))
  
  odds2[,1] <- NaN
  
  expect_equivalent(build_bet_placement_matrix(probs2, odds2, 0, NA), 
                    matrix(0, nrow = nrow(probs2), ncol = ncol(probs2)))
  
  expect_error(build_bet_placement_matrix(select(probs1, -home), odds1, min_advantage, max_odds))
  expect_error(build_bet_placement_matrix(probs1, select(odds1, -home), min_advantage, max_odds))
  expect_error(build_bet_placement_matrix(probs1, odds1, min_advantage = NA, max_odds))

  expect_error(build_bet_placement_matrix(probs1, odds1, c(min_advantage, min_advantage), max_odds))
  expect_error(build_bet_placement_matrix(probs1, odds1, min_advantage, c(max_odds, max_odds))) 
  expect_error(build_bet_placement_matrix(probs1, odds1, "cat", max_odds)) 
  expect_error(build_bet_placement_matrix(probs1, odds1, min_advantage, "dog")) 

})
