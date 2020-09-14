
#' Probit Fit
#' 
#' A wrapper for the polr function with the specs required for order probit model
#' 
#' @param model_data output from probit_build_model_data function
#' 
#' @return fitted model


probit_fit <- function(model_data){
  
  # Polr gives a warning message when it calls glm with probabilistic weights
  
  # suppressWarnings(
  #   polr(result ~ location + attack + defence, data = model_data, weights = time_weight)
  # )
  
  
 suppressWarnings(
   
   clm(result ~ location + attack + defence, data = model_data, weights = time_weight)
   
 )
 
  

}
