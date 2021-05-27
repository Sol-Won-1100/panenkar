
context("Is Same Size")

test_that("is_same_size works as expected", {
  
  x <- matrix(runif(10), nrow = 5)
  y <- tibble(x1 = runif(5), x2 = runif(5))
  z <- data.frame(cat = rnorm(5), dog = letters[1:5])
  a <- matrix(runif(20), nrow = 4)

  expect_equivalent(is_same_size(x, y), TRUE)
  expect_equivalent(is_same_size(x, y, z), TRUE)
  expect_equivalent(is_same_size(x, a), FALSE)
  expect_equivalent(is_same_size(x, y, a), FALSE)
  
  expect_error(is_same_size(x))
  expect_error(is_same_size(x, NA))
  expect_error(is_same_size(x, 12))
  expect_error(is_same_size(x, list(x, y)))
  
})