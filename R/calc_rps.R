
#' Calculate Ranked Probability Score
#' 
#' Calculate the ranked probability score
#' 
#' @param predictions matrix, data.frame or tibble of probabilties, columns are 
#' outcomes and rows are observations
#' @param observed same as predictions, typically a 1 indicates the outcome 
#' occured and 0 it didnt though you could use bookmakers implied probabilties
#' 
#' @return vector of scores
#' 
#' @export

calc_rps <- function(predictions, observed){
  
  predictions <- as.matrix(predictions)
  observed <- as.matrix(observed)
  
  ncat <- ncol(predictions)
  npred <- nrow(predictions)
  
  rps <- numeric(npred)
  
  for (rr in 1:npred){
    obsvec <- rep(0, ncat)
    obsvec[observed[rr]] <- 1
    cumulative <- 0
    for (i in 1:ncat){
      cumulative <- cumulative + (sum(predictions[rr,1:i]) - sum(obsvec[1:i]))^2
    }
    rps[rr] <- (1/(ncat-1))*cumulative
  }
  
  return(rps)
}