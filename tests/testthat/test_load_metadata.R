
context("Load Metadata")

test_that("test load_metadata works correctly", {
  
  metadata <- load_metadata()
  
  expect_equivalent(length(metadata), 3)
  expect_equivalent(names(metadata), c("competitions", "main_leagues", "extra_leagues"))
  
  competitions <- metadata$competitions
  main_leagues <- metadata$main_leagues
  extra_leagues <- metadata$extra_leagues
    
  ids1 <- c(main_leagues$competition_id, extra_leagues$competition_id)
  ids2 <- competitions$competition_id
  
  expect_equivalent(ids1, ids2)
      
})
