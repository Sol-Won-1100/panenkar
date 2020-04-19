
#' @title Previous Season
#'
#' @description Get previous season_id
#'
#' @param season_id season_id in form XXXX_XXXX or XXXX
#' 
#' @return season_id
#' @export

previous_season <- Vectorize(function(season_id){
  
  if(is.numeric(season_id)){
    season_id_previous <- season_id - 1
    
  } else {
    start_season <- season_id %>% str_sub(1, 4) %>% as.numeric()
    
    season_id_previous <- paste0(start_season - 1, "_", start_season)
  }
  
  return(season_id_previous)
})
