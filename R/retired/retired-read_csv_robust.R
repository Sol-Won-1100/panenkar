
#' Read CSV File with Check
#'
#' Read's in a CSV file with an error handler built in incase of failure.
#'
#' @param ... see arguments of dplyr::read_csv
#'
#' @return Outputs a list with elements named "csv", "error" and "error_message"
#' so that you can access the list to check if an error has occurred. If it has
#' occurred then you can throw a stop using the error_message to halt your code
#' otherwise extract the csv element, ideal for writing more robust code. This
#' also means you can suppress warnings around read_csv without fear of missing
#' something.
#'
#'
#' @examples
#' 
#' file = "C:/Users/Neil/Documents/football-betting/data-raw/arg_pd_all.csv"
#'
#' suppressWarnings(
#'   arg_pd <- read_csv_robust(file)
#' )
#'
#' if(arg_pd$error == TRUE){
#'   stop(arg_pd$error_message)
#' } else {
#'   arg_pd <- arg_pd$csv
#' }

read_csv_robust <- function(...){
  
  tryCatch(
    expr = {
      x <- read_csv(...)
      l <- list(x, FALSE, NA)
      names(l) <- c("csv", "error", "error_message")
      return(l)
    },
    
    error = function(e){
      
      error_message <- paste("ERROR: file not found:", file)
      l <- list(NA, TRUE, error_message)
      names(l) <- c("csv", "error", "error_message")
      return(l)
      
    }
  )
}