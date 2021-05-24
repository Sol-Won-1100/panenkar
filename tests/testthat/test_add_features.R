
context("Create Database Table: Results Main")

test_that("add_empty_stadiums works correctly", {
  
  # Test when dates are present things work correctly
  
  dummy_metadata <- panenkar::load_metadata()$competition %>% 
    select(competition_id, start_date_covid_empty_stadium:match_ids_partial_empty_stadium) %>%
    filter(competition_id == "sco_prem") %>%
    mutate(start_date_covid_empty_stadium = ymd("2021-05-12"),
           end_date_covid_empty_stadium = ymd("2021-05-15"),
           start_date_covid_partial_empty_stadium = ymd("2021-05-15"),
           end_date_covid_partial_empty_stadium = ymd("2021-05-15"))
  
  results_pre_add_empty <- glue("{here::here()}/tests/testthat/results_sco_prem.rds") %>% read_rds() 
  
  results <- add_empty_stadiums(results_pre_add_empty, metadata_competitions = dummy_metadata)
  
  expect_equivalent(results$is_empty_stadium, c(rep(TRUE, 7), rep(FALSE, 3)))
  expect_equivalent(results$is_partial_empty_stadium, c(rep(FALSE, 4),rep(TRUE, 3), rep(FALSE, 3)))
  
  # Test when dates are NA things work correctly
  
  dummy_metadata <- dummy_metadata %>%
    mutate(start_date_covid_empty_stadium = NA_Date_,
           end_date_covid_empty_stadium = NA_Date_,
           start_date_covid_partial_empty_stadium = NA_Date_,
           end_date_covid_partial_empty_stadium = NA_Date_)
  
  results <- add_empty_stadiums(results_pre_add_empty, metadata_competitions = dummy_metadata)
  
  expect_equivalent(results$is_empty_stadium, rep(FALSE, 10))
  expect_equivalent(results$is_partial_empty_stadium, rep(FALSE, 10)) 
  
  # Test when end date is NA means until present i.e. still empty
  
  dummy_metadata <- dummy_metadata %>% mutate(start_date_covid_empty_stadium = ymd("2021-05-12"))
  
  results <- add_empty_stadiums(results_pre_add_empty, metadata_competitions = dummy_metadata)

  expect_equivalent(results$is_empty_stadium, rep(TRUE, 10))
  
  # Test when match_ids are specified in metadata then these show
  
  dummy_metadata <- dummy_metadata %>%
    mutate(start_date_covid_empty_stadium = NA_Date_,
           match_ids_empty_stadium = '"sco_prem__20210512__Celtic__St Johnstone"; "sco_prem__20210512__Aberdeen__Hibernian"',
           match_ids_partial_empty_stadium = '"sco_prem__20210512__Celtic__St Johnstone"',
    )
  
  results <- add_empty_stadiums(results_pre_add_empty, metadata_competitions = dummy_metadata)
  
  expect_equivalent(results$is_empty_stadium, c(rep(TRUE, 2), rep(FALSE, 8)))
  expect_equivalent(results$is_partial_empty_stadium, c(rep(TRUE, 1), rep(FALSE, 9)))

  
})

test_that("add_matches_played works as expected", {
  
  file_results <- glue::glue("{load_wd()$tests_testthat}/sco_prem_1994_1995.rds")

  results <- file_results %>% 
    read_rds() %>%
    mutate(match_date = ymd(match_date),
           season_id = if_else(row_number() < 90, "1993_1994", season_id)) %>%
    add_matches_played()
  
  aberdeen_matches <- results %>% 
    pivot_longer(cols = c("home_team", "away_team"), names_to = "location", values_to = "team") %>%
    mutate(matches_played = if_else(location == "home_team", home_matches_played_season, 
                                    away_matches_played_season)) %>%
    filter(team == "Aberdeen") %>%
    select(match_date, season_id, team, location, matches_played)
  
  matches_played_9394 <- aberdeen_matches %>%
    filter(season_id == "1993_1994") %>%
    select(matches_played) %>%
    unlist()
  
  matches_played_9495 <- aberdeen_matches %>%
    filter(season_id == "1994_1995") %>%
    select(matches_played) %>%
    unlist()
  
  expect_equivalent(matches_played_9394, 1:length(matches_played_9394))
  expect_equivalent(matches_played_9495, 1:length(matches_played_9495))
})


