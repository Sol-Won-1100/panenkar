
context("Remove margin")


test_that("remove_margin works as expected", {
  
  create_dummy_odds <- function (n, margin, .seed = NA) {
    
    if (!is.na(.seed)) {
      
      set.seed(.seed)
      dummy_probs <- runif(n)
      
    } else {
      
      dummy_probs <- runif(n)
      
    }
    
    dummy_probs <- dummy_probs / sum(dummy_probs)
    dummy_probs <- dummy_probs * 1.025
    dummy_odds <- 1 / dummy_probs
    
  }
  
  odds1 <- create_dummy_odds(3, 0.025)
  odds2 <- create_dummy_odds(3, 0.025)
  
  # Simple examples I know the answer to from Excel
  
  expect_equal(remove_margin(c(2, 3, 5, 16), method = "straight"), 
               c(0.456273764,	0.30418251,	0.182509506,	0.057034221))
  
  expect_equal(remove_margin(c(2, 3, 4), method = "proportional"), 
               c(0.472222222,	0.305555556,	0.222222222))
  
  expect_equal(remove_margin(c(2, 3, 5, 5), method = "proportional"), 
               c(0.441666667,	0.275,	0.141666667,	0.141666667))
  
  # same length input output
  
  expect_equal(length(remove_margin(odds1, method = "straight")), length(odds1)) 
  expect_equal(length(remove_margin(odds1, method = "proportional")), length(odds1))

  
  expect_equal(sum(remove_margin(odds1) == remove_margin(odds2)), 0) # straight v proportional different
  expect_equal(sum(remove_margin(odds1, method = "straight")), 1) # output sums to 1
  expect_equal(sum(remove_margin(odds1, method = "proportional")), 1) # output sums to 1

  ## Test NAs are handled OK 
  
  # Single NAs bad
  
  expect_error(remove_margin(NA_real_)) 
  expect_error(remove_margin(NA)) 
  expect_error(remove_margin(NA_character_)) 
  
  # If one outcome is NA then cant remove margin - return the whole thing as NA
  
  expect_equal(remove_margin(c(odds1, NA_real_)), rep(NA_real_, length(odds1) + 1))
  expect_equal(remove_margin(c(odds1, NA)), rep(NA_real_, length(odds1) + 1))
  
  # If one element > 1 cant remove margin, error
  
  expect_error(remove_margin(c(odds1, 0.9)))
  
  expect_error(remove_margin_vector(NULL))
  expect_error(remove_margin_vector(c("dog", "cat", "12")))
  
  ## Test matrix inputs
  
  odds_m <- matrix(c(odds1, odds2), nrow = 2, byrow = TRUE)

  expect_equivalent(remove_margin(odds_m), 
                    matrix(c(remove_margin(odds1), remove_margin(odds2)), nrow = 2, byrow = TRUE))
  
  expect_equivalent(remove_margin(odds_m, method = "straight"),
                    matrix(c(remove_margin(odds1, method = "straight"), 
                             remove_margin(odds2, method = "straight")), nrow = 2, byrow = TRUE))
  
  # NA in a row will return an NA for that row only
  
  odds_m[1, 1] <- NA
  
  expect_equivalent(remove_margin(odds_m), matrix(c(rep(NA_real_, 3), remove_margin(odds2)), nrow = 2, byrow = TRUE))
  
  odds_m[2, 1] <- NA
  
  expect_equivalent(remove_margin(odds_m), matrix(c(rep(NA_real_, 6)), nrow = 2, byrow = TRUE))
  
  odds_m <- matrix(c(odds1, odds2), nrow = 2, byrow = TRUE)
  colnames(odds_m) <- paste0("o", 1:ncol(odds_m))
  
  expect_equivalent(colnames(remove_margin(odds_m)), colnames(odds_m))
  
  ## Test df and tibble inputs
  
  odds_df <- as.data.frame(odds_m)
  odds_tb <- tibble(odds_df)
  
  expect_equivalent(remove_margin(odds_df), as.data.frame(remove_margin(odds_m)))
  expect_equivalent(remove_margin(odds_df, method = "straight"), 
                    as.data.frame(remove_margin(odds_m, method = "straight")))
  
  expect_equivalent(remove_margin(odds_tb), tibble(as.data.frame(remove_margin(odds_m))))
  
  
  expect_equivalent(remove_margin(odds_tb, method = "straight"), 
                    tibble(as.data.frame(remove_margin(odds_m, method = "straight"))))
  
})
