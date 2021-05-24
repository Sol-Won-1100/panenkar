
#' Load Working Directory
#' 
#' Load in working directory paths
#' 
#' @return working directory list
#' @export

load_wd <- function () {
  
  ## Top Level ---------------------------------------------------------------------------------------------------------
  
  wd <- list()
  
  wd$wd <- glue("{here::here()}/")
  
  ## development -------------------------------------------------------------------------------------------------------
  
  wd$dev <- glue("{wd$wd}development/")
  
  # football_data_website ---
  
  wd$dev_football_data_website <- glue("{wd$dev}football_data_website/")
  
  # result_sco_prem ---
  
  wd$dev_result_sco_prem <- glue("{wd$dev}result_sco_prem/")
  
  wd$dev_result_sco_prem_processed_data <- glue("{wd$dev_result_sco_prem}processed_data/")
  wd$dev_result_sco_prem_raw_data <- glue("{wd$dev_result_sco_prem}raw_data/")
  wd$dev_result_sco_prem_results <- glue("{wd$dev_result_sco_prem}results/")
  wd$dev_result_sco_prem_scripts <- glue("{wd$dev_result_sco_prem}scripts/")
  
  # workflow ---
  
  wd$dev_workflow <- glue("{wd$dev}workflow/")
  
  ## documentation -----------------------------------------------------------------------------------------------------
  
  wd$documentation <- glue("{wd$wd}documentation/")
  
  ## live_data ---------------------------------------------------------------------------------------------------------
  
  wd$live_data <- glue("{wd$wd}live_data/")
  
  wd$live_data_football_data_co_uk_historic_csvs <- glue("{wd$live_data}football_data_co_uk_historic_csvs/")
  wd$live_data_football_data_co_uk_latest_csvs <- glue("{wd$live_data}football_data_co_uk_latest_csvs/")
  wd$metadata <- glue("{wd$live_data}metadata/")
  
  ## man ---------------------------------------------------------------------------------------------------------------
  
  wd$man <- glue("{wd$wd}man/")
 
  ## production --------------------------------------------------------------------------------------------------------
  
  wd$production <- glue("{wd$wd}production/")
  
  wd$production_record <- glue("{wd$production}record/")
  
  ## R -----------------------------------------------------------------------------------------------------------------
  
  wd$r <- glue("{wd$wd}R/")
  
  ## tests -------------------------------------------------------------------------------------------------------------
  
  wd$tests <- glue("{wd$wd}tests/")
  
  wd$tests_testthat <- glue("{wd$tests}testthat/")
  
  wd$tests_testthat_snap <- glue("{wd$tests_testthat}_snaps/")
  
  return(wd)
  
}
