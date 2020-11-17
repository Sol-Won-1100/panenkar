
context("Goal Difference Model")

# Setup ----------------------------------------------------------------------------------------------------------------

file_results <- here::here() %>% paste0("/tests/testthat/sco_prem_1994_1995.rds")

results <- read_rds(file_results)
results$match_date <- ymd(results$match_date)

Tests ----------------------------------------------------------------------------------------------------------------

test_that("gd_calc_rating works as expected", {

  match_rating1 <- gd_calc_rating(results, "Celtic",  ymd("1995-07-07"), xi = 0.0016)
  match_rating2 <- gd_calc_rating(results, "Celtic",  ymd("1995-07-07"), xi = 0.0016, max_gd = 20)
  
  expect_equal(is.numeric(match_rating1), TRUE)
  expect_equal(length(match_rating1), 1)
  expect_equal(match_rating1, match_rating2)
  
  expect_equal(gd_calc_rating(results, "Cardiff",  ymd("1995-07-07"), xi = 0.0016), NA_real_)

  expect_error(gd_calc_rating(results, c("Celtic", "Hearts"),  ymd("1995-07-07"), xi = 0.0016))
  expect_error(gd_calc_rating(tibble("blah"), "Celtic",  ymd("1995-07-07"), xi = 0.0016))
  expect_error(gd_calc_rating(12, "Celtic",  ymd("1995-07-07"), xi = 0.0016))
  expect_error(gd_calc_rating(results, "Celtic", rep(ymd("1995-07-07"), 2), xi = 0.0016))
  expect_error(gd_calc_rating(results, "Celtic", ymd("1995-07-07"), xi = c(1,1)))

})

test_that("gd_calc_ratings works as expected", {
  
  match_rating1 <- gd_calc_rating(results, "Celtic",  ymd("1995-07-07"), xi = 0.0016)
  match_rating2 <- gd_calc_rating(results, "Hearts",  ymd("1995-07-07"), xi = 0.0016, max_gd = 20)
  
  gd_calc_ratings(results, c("Celtic", "Hearts"),  ymd("1995-07-07"), xi = 0.0016) %>%
    expect_equal(c(match_rating1, match_rating2))
  
  expect_error(gd_calc_ratings(results, 1:4,  ymd("1995-07-07"), xi = 0.0016))

})

test_that("gd_add_ratings works as expected", {
  
  # Test the structure of the output tibble
  # Compare with gd_calc_rating at a particular point in time
  
})





