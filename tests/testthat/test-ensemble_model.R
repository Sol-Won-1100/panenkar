
context("Ensemble")


test_that("build_ensemble works as expected", {
  
  
  predictions1 <- matrix(c(0.5, 0.3, 0.2, 0.6, 0.3, 0.1), byrow = TRUE, nrow = 2)
  predictions2 <- matrix(c(0.55, 0.25, 0.2, 0.55, 0.3, 0.15), byrow = TRUE, nrow = 2)
  predictions <- list(predictions1, predictions2)
  predictions_df <- map(predictions, as.data.frame)
  
  expect_equal(build_ensemble(predictions, c(1, 0)), predictions1)
  expect_equal(build_ensemble(predictions, c(0, 1)), predictions2)
  expect_equal(build_ensemble(predictions, c(1, 1)), build_ensemble(predictions, c(0.5, 0.5)))
  
  expect_equal(build_ensemble(list(predictions1, predictions1), c(0.6, 0.4)), predictions1)
  expect_equal(build_ensemble(predictions, c(0.4, 0.6)), build_ensemble(predictions_df, c(0.4, 0.6)))
  
  predictions %>%
    build_ensemble(c(0.55, 0.45)) %>%
    rowSums() %>%
    expect_equal(c(1,1))
  
  expect_error(build_ensemble(list(predictions1), c(0.5, 0.5)))
  expect_error(build_ensemble("cat", c(0.5, 0.5)))
  
})


test_that("calc_ensemble_weights works as expected", {
  
  set.seed(241)
  
  predictions1 <- runif(100) %>% 
    tibble(outcome1 = .) %>% 
    mutate(outcome2 = 1 - outcome1)
  
  set.seed(223700)
  
  predictions2 <- runif(100) %>% 
    tibble(outcome1 = .) %>% 
    mutate(outcome2 = 1 - outcome1)
  
  set.seed(221)
  
  observed <- runif(100) %>% 
    tibble(outcome1 = .) %>% 
    mutate(outcome2 = 1 - outcome1)  
  
  predictions <- list(predictions1, predictions2)
  
  optimised_weights <- calc_ensemble_weights(predictions, observed)
  
  expect_equal(length(optimised_weights), 2)
  expect_equal(sum(optimised_weights), 1)
  expect_error(calc_ensemble_weights(predictions[1], observed)) 
  expect_error(calc_ensemble_weights(predictions, "observed"))   
  
})


test_that("optim_ensemble works as expected", {
  
  set.seed(241)
  
  predictions1 <- runif(100) %>% 
    tibble(outcome1 = .) %>% 
    mutate(outcome2 = 1 - outcome1)
  
  set.seed(223700)
  
  predictions2 <- runif(100) %>% 
    tibble(outcome1 = .) %>% 
    mutate(outcome2 = 1 - outcome1)
  
  set.seed(221)
  
  observed <- runif(100) %>% 
    tibble(outcome1 = .) %>% 
    mutate(outcome2 = 1 - outcome1)  
  
  predictions <- list(predictions1, predictions2)

  optimised_weights <- calc_ensemble_weights(predictions, observed)
  
  expect_lt(optim_ensemble(optimised_weights, predictions, observed), optim_ensemble(c(1, 1), predictions, observed))

})



