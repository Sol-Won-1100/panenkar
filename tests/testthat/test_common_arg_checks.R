
testthat::context("Common Argument Checks")

testthat::test_that("check_arg_results works as expected", {
  
  testthat::expect_error(check_arg_results("blah", c("home_team", "away_team")))
  expect_error(check_arg_results(dplyr::tibble("blah"), c("home_team", "away_team")))
  
  x <- tibble(home_team = "blah", away_team = "foo")
  
  testthat::expect_equal(check_arg_results(x, c("home_team", "away_team")), x)
  expect_equal(check_arg_results(x, "home_team"), x)
  expect_error(check_arg_results(x, c("home_team", "bla")))
  expect_error(check_arg_results(12, c("home_team", "bla")))
  expect_error(check_arg_results(NA, c("home_team", "bla")))
  expect_error(check_arg_results(NULL, c("home_team", "bla")))
  
})


test_that("check_arg_xi works as expected", {
  
  expect_error(check_arg_xi("cat"))
  expect_error(check_arg_xi(c(0.0016, 0.0017)))
  expect_equal(check_arg_xi(0.05), 0.05)
    
})


test_that("check_arg_current_date works as expected", {
  
  expect_equal(check_arg_current_date(Sys.Date()), Sys.Date())
  
  expect_error(check_arg_current_date("cat"))
  
  expect_error(check_arg_current_date(c(Sys.Date() - 1, Sys.Date())))

})

