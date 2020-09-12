
#' Poisson Fit
#' 
#' A wrapper for the glm function with the specs required for poisson regression model
#' 
#' @param model_data output from poisson_build_model_data function
#' 
#' @return fitted model


poisson_fit <- function(model_data){
  
  glm(goals ~ location + attack + defence, family = poisson(link = log), data = model_data, weight = time_weight)
  
}