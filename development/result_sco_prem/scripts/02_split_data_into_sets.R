

# TITLE:       Split Data Into Sets
# DESCRIPTION: Split the data into initial training, model fitting, test and closing odds sets

# Setup ----------------------------------------------------------------------------------------------------------------

.competition_id <- "sco_prem"
wd <- load_wd()


# Get data and split into sets -----------------------------------------------------------------------------------------

results <- "{wd$dev_result_sco_prem_raw_data}results_main_{.competition_id}.rds" %>%
  glue() %>% 
  read_rds()

results %>% filter(!is.na(home_odds_sharp_closing))


# The first set is for initializing models. 
# The second set is for fitting models to and combining or what have you.
# The third set is for testing the models for profitability.
# The fourth set is for testing performance against the closing line. This is a subset to the third set because we dont
# have as much closing line data.

season_ids_initial_training <- sequence_seasons("1994_1995", "1999_2000")
season_ids_model_fitting <-  sequence_seasons("2000_2001", "2006_2007")
season_ids_test_main <- sequence_seasons("2007_2008", "2018_2019")
season_ids_test_closing <- sequence_seasons("2012_2013", "2018_2019")

set_inital_training <- filter(results, season_id %in% season_ids_initial_training)
set_model_fitting <- filter(results, season_id %in% season_ids_model_fitting)
set_test_main <- filter(results, season_id %in% season_ids_test_main)
set_test_closing <- filter(results, season_id %in% season_ids_test_closing)

set_inital_training %>% write_rds(file = glue("{wd$dev_result_sco_prem_processed_data}set_inital_training.rds"))

# Odds start here
set_model_fitting %>% write_rds(file = glue("{wd$dev_result_sco_prem_processed_data}set_model_fitting.rds"))
set_test_main %>% write_rds(file = glue("{wd$dev_result_sco_prem_processed_data}set_test_main.rds"))
set_test_closing %>% write_rds(file = glue("{wd$dev_result_sco_prem_processed_data}set_test_closing.rds"))

