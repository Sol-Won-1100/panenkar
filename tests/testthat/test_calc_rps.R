
context("Ranked probability score (RPS)")

test_that("calc_rps works as expected", {
  
  predicted <- matrix(c(0.8, 0.1, 0.1, 0.3, 0.35, 0.35), byrow = TRUE, nrow = 2)
  observed <- matrix(c(1, 0, 0, 0, 0, 1), byrow = TRUE, nrow = 2)
  
  predicted_df <- data.frame(predicted)
  observed_df <- data.frame(observed)
  predicted_tbl <- as_tibble(predicted_df)
  observed_tbl <- as_tibble(observed_df)
  
  # Tibbles, dfs and matrices all work the same
  
  expect_equal(calc_rps(predicted, observed), calc_rps(predicted_df, observed_df))
  expect_equal(calc_rps(predicted, observed), calc_rps(predicted_tbl, observed_tbl))
  
  # More accurate prediction returns lower RPS score
  
  expect_lt(calc_rps(predicted_df[1, ], observed_df[1, ]), calc_rps(predicted_df[2, ], observed_df[2, ]))
  
  # Exact predictions return 0
  
  expect_equal(calc_rps(data.frame(1, 0, 0), data.frame(1, 0, 0)), 0)
  
  expect_error(calc_rps(predicted, "cat"))
  expect_error(calc_rps(1:10, observed))
  expect_error(calc_rps(predicted, observed, "bat"))
  
    
})
