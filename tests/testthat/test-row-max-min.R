
context("Row Max Min")

test_that("row max works as expected", {
  
  x <- tribble(~x1,  ~x2, ~x3,
               "dog", 2,   4)
  
  y <- tribble(~x1,  ~x2, ~x3,
               "dog", 2,   4,
               "cat", 11, 12) 
  
  y_append <- mutate(y , y_row_max = c(4, 12))
  
  expect_equal(row_max(x,x2, x3, append_col = FALSE), 4)
  expect_equal(row_max(y,x2, x3, append_col = FALSE), c(4, 12))
  expect_equal(row_max(y,x2, x3, new_col_name = "y_row_max"), y_append)
  
})


test_that("row min works as expected", {
  
  x <- tribble(~x1,  ~x2, ~x3,
               "dog", 2,   4)
  
  y <- tribble(~x1,  ~x2, ~x3,
               "dog", 2,   4,
               "cat", 11, 12) 
  
  y_append <- mutate(y , y_row_min = c(2, 11))
  
  expect_equal(row_min(x,x2, x3, append_col = FALSE), 2)
  expect_equal(row_min(y,x2, x3, append_col = FALSE), c(2, 11))
  expect_equal(row_min(y,x2, x3, new_col_name = "y_row_min"), y_append)
  
})


test_that("cols not in x specified via ... returns error", {
  
  x <- tribble(~x1,  ~x2, ~x3,
               "dog", "cat",   "mouse")
  
  expect_error(row_max(x, x4))
  expect_error(row_min(x, x4))
})


test_that("no numeric columns returns error", {
  
  x <- tribble(~x1,  ~x2, ~x3,
               "dog", "cat",   "mouse")
  
  y <- tribble(~x1,  ~x2, ~x3,
               "dog", 2,   4,
               "cat", 11, 12) 
  
  expect_error(row_max(x))
  expect_error(row_max(y, x1))
  expect_error(row_min(x))
  expect_error(row_min(y, x1))
})




