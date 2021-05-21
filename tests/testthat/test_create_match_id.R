
context("Create Match ID")

competition_id <- "sco_pl"
match_date <- ymd("2020-01-01")
home_team <- "Celtic"
away_team <- "Rangers"
match_id <- "sco_pl__20200101__Celtic__Rangers"

test_that("create_match_id works as expected", {

  expect_equivalent(create_match_id(competition_id, match_date, home_team, away_team), match_id)

})

test_that("error handling works as expected", {
  
  # library(lubridate)
  # 
  # competition_id <- "sco_pl"
  # match_date <- ymd("2020-01-01")
  # home_team <- "Celtic"
  # away_team <- "Rangers"
  # match_id <- "sco_pl__20200101__Celtic__Rangers"
  
  expect_error(create_match_id(competition_id, as.character(match_date), home_team, away_team))
  expect_warning(create_match_id(competition_id, match_date, home_team, away_team = home_team))
  
})


test_that("if one argument is NA return NA", {

  # competition_id <- "sco_pl"
  # match_date <- ymd("2020-01-01")
  # home_team <- "Celtic"
  # away_team <- "Rangers"
  # match_id <- "sco_pl__20200101__Celtic__Rangers"
  
  expect_equivalent(create_match_id(NA, match_date, home_team, away_team), NA_character_)
  expect_equivalent(create_match_id(competition_id, NA, home_team, away_team), NA_character_)
  expect_equivalent(create_match_id(competition_id, match_date, NA, away_team), NA_character_)
  expect_equivalent(create_match_id(competition_id, match_date, home_team, NA), NA_character_)
  
})