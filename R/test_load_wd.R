
context("Load Working Directory")

test_that("test load_wd works correctly", {

  all_folders <- here::here() %>% list.dirs()
  folders <- all_folders %>% str_detect(".git|.Rproj", negate = TRUE) %>% magrittr::extract(all_folders, .) %>% sort()
  folders <- paste0(folders[folders != "C:/Users/neil_/Desktop/Files/betting/football/panenkar/R/to-do"], "/")
 
  wd_folders <- load_wd() %>% unlist() %>% sort() %>% unname()
  
  expect_equivalent(folders, wd_folders)
  
})
