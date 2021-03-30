
context("Remove margin")

test_that("remove_margin output sums to 1", {

  dummy_probs <- runif(1, min = 0, max = 20) %>% ceiling() %>% runif()
  dummy_probs <- dummy_probs / 1
  dummy_probs <- dummy_probs * 1.025
  dummy_odds <- 1 / dummy_probs
  
  expect_equal(sum(remove_margin(dummy_odds, method = "straight")), 1)
  expect_equal(sum(remove_margin(dummy_odds, method = "proportional")), 1)

    
})

test_that("remove_margin output length is same as input", {

  expect_equal(length(remove_margin(dummy_odds, method = "straight")), length(dummy_odds))
  
})

test_that("remove_margin error handling works as expected", {
  
  expect_error(remove_margin(NA))
  expect_error(remove_margin(NA_real_))
  expect_error(remove_margin(NULL))
  expect_error(remove_margin(c("dog", "cat", "12")))
  expect_error(remove_margin(c(5, 4, NA)))
  expect_error(remove_margin(c(5, 4, 0.1)))
  
})
