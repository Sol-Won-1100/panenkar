
context("Build Indicator Matrix")

test_that("build_indicator_matrix works as expected", {
  
  outcomes <- c("home", "draw", "away")
  
  set.seed(14)
  x <- factor(sample(outcomes, 5, TRUE), levels = outcomes)
  
  expected_indicator_matrix <- matrix(c(1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0), nrow = 5, byrow = TRUE)
  colnames(expected_indicator_matrix) <- outcomes
  
  expect_equal(build_indicator_matrix(x), expected_indicator_matrix)
  
  x[1] <- NA
  expected_indicator_matrix[1, ] <- NA
  
  expect_equal(build_indicator_matrix(x), expected_indicator_matrix)
  
  expect_error(build_indicator_matrix(outcomes))  

})





