
context("Is_...")

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

test_that("is_plain_vector works as expected", {
  
  expect_equivalent(is_plain_vector(1:10), TRUE)
  expect_equivalent(is_plain_vector(c(1.5, 2.4, 12)), TRUE)
  expect_equivalent(is_plain_vector(c("dog", "cat")), TRUE)
  expect_equivalent(is_plain_vector(c(TRUE, FALSE)), TRUE)
  
  expect_equivalent(is_plain_vector(list(1:10)), FALSE)
  expect_equivalent(is_plain_vector(matrix(1:10)), FALSE)
  expect_equivalent(is_plain_vector(data.frame(1:10)), FALSE)
  
})

test_that("is_matrix_df_tibble works as expected", {
  
  expect_equivalent(is_matrix_df_tibble(matrix(1:10)), TRUE)
  expect_equivalent(is_matrix_df_tibble(data.frame(1:10)), TRUE)
  expect_equivalent(is_matrix_df_tibble(tibble(1:10)), TRUE)
  
  expect_equivalent(is_matrix_df_tibble(c(1.5, 2.4, 12)), FALSE)
  expect_equivalent(is_matrix_df_tibble(c("dog", "cat")), FALSE)
  expect_equivalent(is_matrix_df_tibble(c(TRUE, FALSE)), FALSE)
  expect_equivalent(is_matrix_df_tibble(list(1:10)), FALSE)

})

