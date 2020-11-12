
context("Season IDs")

test_that("error check season id returns TRUE and FALSE as desired", {
  
  # Valid
  
  expect_equal(is_season_id("2040_2041"), TRUE)
  expect_equal(is_season_id("1999_2000"), TRUE)
  expect_equal(is_season_id("1980"), TRUE)
  
  # Out of bounds
  
  expect_equal(is_season_id("1700"), FALSE)  
  expect_equal(is_season_id("3008"), FALSE)  
  expect_equal(is_season_id("3058_3059"), FALSE)
  expect_equal(is_season_id("1058_1059"), FALSE)
  
  # Bad type
  
  expect_equal(is_season_id(2020), FALSE)
  expect_equal(is_season_id(NA), FALSE) 
  expect_equal(is_season_id(NULL), FALSE) 
  expect_equal(is_season_id(NA_character_), FALSE) 
  
  expect_equal(is_season_id(Sys.Date()), FALSE)  
  expect_equal(is_season_id(tibble(x = 1700)), FALSE)  
  expect_equal(is_season_id(matrix(1700)), FALSE)  
  expect_equal(is_season_id(data.frame(1700)), FALSE)  
  
  # Bad numbers
  
  expect_equal(is_season_id(17.5), FALSE)    
  expect_equal(is_season_id(2000.5), FALSE)    
  
  
  # Bad formats
  
  expect_equal(is_season_id("2000_1999"), FALSE)
  expect_equal(is_season_id("2000_2002"), FALSE)
  expect_equal(is_season_id("2000/2001"), FALSE)

  # Evaluate multiple works correctly
  suppressWarnings(
    expect_equal(is_season_id(c("2000_2001", "2001_2002")), FALSE)
  )
  
  expect_warning(is_season_id(c("2000_2001", "2001_2002")))
  
})

test_that("previous_season works as expected", {

  expect_equal(previous_season(2018), NA_character_)
  expect_equal(previous_season("2018"), "2017")
  expect_equal(previous_season("2018_2019"), "2017_2018")  
  expect_equal(previous_season(c("2018", "2017_2018")), c("2017",  "2016_2017"))  
  
  expect_equal(previous_season(c("20189", "2017_2018")), c(NA_character_, "2016_2017"))
  expect_equal(previous_season("20189"), NA_character_)
  
})

test_that("max and min season work as expected", {
  
  season_ids_multi_year <- c("2018_2019", "2015_2016", "2019_2020", "2018_2019")
  
  expect_equal(min_season(season_ids_multi_year), "2015_2016")
  expect_equal(max_season(season_ids_multi_year), "2019_2020")
  
  season_ids_single_year <- 2010:2021 %>% as.character()
  
  expect_equal(min_season(season_ids_single_year), "2010")
  expect_equal(max_season(season_ids_single_year), "2021")  
  
  expect_error(min_season(1:100))
  expect_error(max_season(NA))
  
})

test_that("sequence_season work as expected", {
  
  
  multi_year_seq <- c("2015_2016", "2016_2017", "2017_2018", "2018_2019")
  single_year_seq <- as.character(2010:2020)
  
  expect_equal(sequence_seasons("2015_2016", "2018_2019"), multi_year_seq)
  expect_equal(sequence_seasons("2010", "2020"), single_year_seq)
  
  expect_error(sequence_seasons(2015, "2018"))
  expect_error(sequence_seasons("2018", "2015"))
  
  suppressWarnings(
    expect_error(sequence_seasons(c("2016", "2017"), "2020"))
  )
  
  suppressWarnings(
    expect_error(sequence_seasons("2016_2017", c("2020_2021", "2021_2022")))
  )
  
})


