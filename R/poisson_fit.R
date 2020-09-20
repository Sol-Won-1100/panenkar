
#' Poisson Fit
#' 
#' A wrapper for the glm function with the specs required for poisson regression model and similar for the zero inflated
#' poisson model
#' 
#' @param model_data output from poisson_build_model_data function
#' @param zero_inflated default FALSE, if TRUE will use zero inflated poisson
#' 
#' @return fitted model


poisson_fit <- function(model_data, zero_inflated = FALSE){

  if (zero_inflated == FALSE) {
    
    glm(goals ~ location + attack + defence, family = poisson(link = log), data = model_data, weight = time_weight)
    
  } else {
    
    suppressWarnings(
      zeroinfl(goals ~ location + attack + defence, data = model_data, weight = time_weight)
    ) # Often gives warnings about sub-standard fits
    
    
  }
  
  
}