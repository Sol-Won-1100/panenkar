
# TITLE:       Run Poisson Model

# Setup ----------------------------------------------------------------------------------------------------------------

tic()

.competition_id <- "sco_prem"
wd <- load_wd()

set_inital_training <- read_rds(glue("{wd$dev_result_sco_prem_processed_data}set_inital_training.rds"))
set_model_fitting <- read_rds(glue("{wd$dev_result_sco_prem_processed_data}set_model_fitting.rds"))
set_test_main <- read_rds(glue("{wd$dev_result_sco_prem_processed_data}set_test_main.rds"))

## Run model -----------------------------------------------------------------------------------------------------------

predicted_poisson_model_fitting <- poisson_simulate_matches(set_inital_training, set_model_fitting, 
                                                            markets = "result")

predicted_poisson_test_main <- set_inital_training %>% 
  bind_rows(set_model_fitting) %>%
  poisson_simulate_matches(set_test_main, markets = "result")

predicted_poisson_model_fitting %>% 
  write_rds(file = glue("{wd$dev_result_sco_prem_processed_data}predicted_poisson_model_fitting.rds"))

predicted_poisson_test_main %>% 
  write_rds(file = glue("{wd$dev_result_sco_prem_processed_data}predicted_poisson_test_main.rds"))

toc() # 3 mins 14 seconds to run