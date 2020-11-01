
context("Testing functions for connecting to access database")

# Delete these 2 once complete
library(panenkar)
library(testthat)

test_that("errors returned if file does not exist", {
  
  expect_error(connect_to_access_dbi("path-that-doesnt-exist.accdb"))
  
})

test_that("errors returned if file is wrong type", {
  
  file_name <- here::here() %>% paste0("/tests/testthat/tur_sl_2019_2020.csv")
  
  expect_equal(file.exists(file_name), TRUE)
  expect_error(connect_to_access_dbi(file_name))
  
})



