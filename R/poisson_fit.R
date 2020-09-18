
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

  glm(goals ~ location + attack + defence, family = poisson(link = log), data = model_data, weight = time_weight)
  
}