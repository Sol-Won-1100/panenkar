
context("Remove margin")

dummy_probs <- runif(1, min = 0, max = 20) %>% ceiling() %>% runif()
dummy_probs <- dummy_probs / 1
dummy_probs <- dummy_probs * 1.025
dummy_odds <- 1 / dummy_probs

test_that("remove_margin_vector output sums to 1", {
  
  expect_equal(sum(remove_margin_vector(dummy_odds, method = "straight")), 1)
  expect_equal(sum(remove_margin_vector(dummy_odds, method = "proportional")), 1)

})

test_that("remove_margin_vector output length is same as input", {

  expect_equal(length(remove_margin_vector(dummy_odds, method = "straight")), length(dummy_odds))
  
})

test_that("remove_margin_vector error handling works as expected", {
  
  suppressWarnings(expect_equal(remove_margin_vector(NA), NA_real_))
  suppressWarnings(expect_equal(remove_margin_vector(NA_real_), NA_real_))
  
  suppressWarnings(expect_equal(remove_margin_vector(c(dummy_odds, NA_real_)), rep(NA_real_, length(dummy_odds) + 1)))
  suppressWarnings(expect_equal(remove_margin_vector(c(dummy_odds, 0.9)), rep(NA_real_, length(dummy_odds) + 1)))
  
  expect_warning(remove_margin_vector(NA))
  expect_warning(remove_margin_vector(NA_real_))
  expect_warning(remove_margin_vector(c(5, 4, NA)))
  
  expect_error(remove_margin_vector(NULL))
  expect_error(remove_margin_vector(c("dog", "cat", "12")))
  expect_warning(remove_margin_vector(c(5, 4, 0.1)))
  
})


test_that("remove_margin the wrapper round remove_margin_vector works as expected", {
  
  # Write a test which checks that a 2 row df or matrix remove_margin gives same output as remove_margin_vector pieced
  # together
  
  
})

