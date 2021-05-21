
# TITLE:       Clean Football Data Website CSV Headers
# DESCRIPTION: The names of home and away teams in the greek data are not consistent with the other datasets. This just
#              makes sure all the data is correctly named for easy use later on.

# ----------------------------------------------------------------------------------------------------------------------

wd <- load_wd()

files <- list.files(wd$live_data_football_data_co_uk_historic_csvs, full.names = TRUE)

files_main <- files %>% 
  str_detect(pattern = "_all", negate = TRUE) %>% 
  magrittr::extract(files, .)

datasets <- map(files_main, read_csv)
col_names <- map(datasets, colnames)

replace_col_name <- function (x) {
  
  col_names <- colnames(x)
  
  col_names[col_names == "HT"] <- "HomeTeam"
  col_names[col_names == "AT"] <- "AwayTeam"
  
  colnames(x) <- col_names
  
  return (x)
  
}

datasets_replaced <- map(datasets, replace_col_name)

walk2(datasets_replaced, files_main, write_csv)

