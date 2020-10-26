
context("Create Database Table: Results Main")


# test_that("create_db_table_results_main_fd_main works as expected", {
# 
# })

test_that("add_missing_col_name works as expected", {
  
  x <- tribble(~x1,  ~x2, ~x3,
               "dog", 2,   4)
  
  y <- mutate(x, x4 = NA_real_)
  
  expect_equal(add_missing_col_name(x, "x1"), x)
  expect_equal(add_missing_col_name(x, "x4"), y)
  
})


test_that("calc_max_odds works as expected", {
  
  x <- tribble(~home_odds_coral,  ~home_odds_william_hill, ~away_odds_coral,
               4.5,               4.2,                     4.8,
               NA_real_,          2,                       2.2)

  expect_equal(calc_max_odds(x, "home_odds"), c(4.5, 2))

})


# test_that("missing file flags error", {
#   
# })
# 
# test_that("bad file name flags error", {
#   
# })
