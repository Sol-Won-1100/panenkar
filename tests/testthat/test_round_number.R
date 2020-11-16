
context("Round Number")


test_that("round_number works as expected", {
  
  expect_equal(round_number(c(1.5, 2.5, 3.5)), 2:4)
  expect_equal(round_number(c(-10.5, -11.5, -12.5)), (-11):(-13))
  
  set.seed(1212)
  x <- runif(10, -100, 100)
  
  expect_equal(round(x), round_number(x))
  expect_equal(round(x, 3), round_number(x, 3))
  expect_equal(round(0), round_number(0))
  
})
