
#' Result
#'
#' Given a score, calculate the result
#'
#' @param home_goals goals scored by the home team
#' @param away_goals goals scored by the away team
#' @return The result, a character vector with possible values "home", "draw" or
#' "away"
#' @export
#' @examples
#' result(2, 1)

result <- Vectorize(function(home_goals, away_goals){
  
  if(is.na(home_goals)){
    return(NA)
  } else if(is.na(away_goals)){
    return(NA)
  }else if(home_goals > away_goals){
    return("home")
  } else if(home_goals == away_goals){
    return("draw")
  } else if(home_goals < away_goals){
    return("away")
  } else{
    return(NA)
  }
  
})
