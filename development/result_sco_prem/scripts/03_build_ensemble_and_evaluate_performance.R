


# Reading in needs the glue treatment and moved to set 3.

predicted_poisson <- wd$output %>% paste0("predicted_poisson.rds") %>% read_rds()



predicted_probit <- wd$output %>% paste0("predicted_probit.rds") %>% read_rds()