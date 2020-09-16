
#' Poisson Fit
#' 
#' A wrapper for the glm function with the specs required for poisson regression model
#' 
#' @param model_data output from poisson_build_model_data function
#' 
#' @note To make more robust do a check if attack_promoted_into is there but not defence_promoted_into and vice versa 
#' 
#' @return fitted model


poisson_fit <- function(model_data){
  
  col_names <- colnames(model_data)
  
  promoted_var_in_model <- "attack_promoted_into" %in% col_names & "defence_promoted_into" %in% col_names
  relegated_var_in_model <- "attack_relegated_into" %in% col_names & "defence_relegated_into" %in% col_names
  
  if (promoted_var_in_model == TRUE & relegated_var_in_model == TRUE) {
    
    f <- formula(goals ~ location + attack + defence + attack_promoted_into + defence_promoted_into + 
                         attack_relegated_into + defence_relegated_into)
    
    
  } else if (promoted_var_in_model == TRUE) {
    
    f <- formula(goals ~ location + attack + defence + attack_promoted_into + defence_promoted_into)
    
  } else if (relegated_var_in_model == TRUE) {
    
    f <- formula(goals ~ location + attack + defence + attack_relegated_into +  defence_relegated_into)    
    
  } else {
    
    f <- forumla(goals ~ location + attack + defence)
    
  }
  
  glm(f, family = poisson(link = log), data = model_data, weight = time_weight)
  
}