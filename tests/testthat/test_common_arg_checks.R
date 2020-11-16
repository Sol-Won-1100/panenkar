
context("Common Argument Checks")

test_that("check_arg_results works as expected", {
  
  expect_error(check_arg_results("blah", c("home_team", "away_team")))
  expect_error(check_arg_results(tibble("blah"), c("home_team", "away_team")))
  
  x <- tibble(home_team = "blah", away_team = "foo")
  
  expect_equal(check_arg_results(x, c("home_team", "away_team")), x)
  expect_equal(check_arg_results(x, "home_team"), x)
  expect_error(check_arg_results(x, c("home_team", "bla")))
  expect_error(check_arg_results(12, c("home_team", "bla")))
  expect_error(check_arg_results(NA, c("home_team", "bla")))
  expect_error(check_arg_results(NULL, c("home_team", "bla")))
  
})




