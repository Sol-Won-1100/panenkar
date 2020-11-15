
context("Goal Difference Model")

# Setup ----------------------------------------------------------------------------------------------------------------

# Delete
library(panenkar)
library(testthat)

file_results <- here::here() %>% paste0("/tests/testthat/sco_prem_1994_1995.rds")

results <- read_rds(file_results)
results$match_date <- ymd(results$match_date)
num_matches <- nrow(results)

fit <- clm(result ~ match_rating, data = results)

# Tests ----------------------------------------------------------------------------------------------------------------

test_that("gd_calc_rating works as expected", {
  
  match_rating <- gd_calc_rating(results, "Celtic",  ymd("1995-07-07"), xi = 0.0016, max_gd = NA, min_matches = 10)


  
  
  
  
    
})

