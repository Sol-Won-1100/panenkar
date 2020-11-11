
context("Ensemble")

# Delete

library(panenkar)
library(testthat)

test_that("build_ensemble works as expected", {
  
  
  predictions1 <- matrix(c(0.5, 0.3, 0.2, 0.6, 0.3, 0.1), byrow = TRUE, nrow = 2)
  predictions2 <- matrix(c(0.55, 0.25, 0.2, 0.55, 0.3, 0.15), byrow = TRUE, nrow = 2)
  predictions <- list(predictions1, predictions2)
  
  expect_equal(build_ensemble(predictions, c(1, 0)), predictions1)
  expect_equal(build_ensemble(predictions, c(0, 1)), predictions2)
  
  expect_equal(build_ensemble(list(predictions1, predictions1), c(0.6, 0.4)), predictions1)
  
  predictions %>%
    build_ensemble(c(0.55, 0.45))
  
  expect_equal(rowSums(build_ensemble()))
})

observed <- matrix(c(0, 1, 0, 1, 0, 0), byrow = TRUE, nrow = 2)

