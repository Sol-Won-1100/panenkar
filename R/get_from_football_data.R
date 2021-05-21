

#' @title Get From Football Data
#'
#' @description Download results and odds data from www.football-data.co.uk
#'
#' @param competition_id See \link[panenkar]{get_metadata} for format details
#' @param season_ids See \link[panenkar]{sequence_seasons} for format details
#' @param path Folder to save the downloaded data into
#'
#' @export
#' @details Will change this to a helper function once more data sources added.
#'
#' @examples
#' get_from_football_data("eng_pl")

get_from_football_data <- function(competition_id, season_ids = NA, path){

  ## Error handling

  if (length(competition_id) > 1) {
    
    competition_id <- competition_id[1]
    warning("competition_id has length > 1")
    
  }

  if (error_check_competition_id(competition_id) == FALSE) {
    
    stop("bad competition_id format supplied see metadata")
    
  }

  if (!is.na(season_ids[1])) {
    
    season_ids_check <- map_lgl(season_ids, error_check_season_id)

    if (length(season_ids_check) != sum(season_ids_check)) {
      
      stop("one or more of season_ids is in a bad format")
      
    }
    
  }

  ## Find competition_id details for downloading

  metadata <- get_metadata(competition_id)
  metadata_football_data <- metadata$football_data
  data_type <- metadata_football_data$data_type

  if (is.na(season_ids[1]) & data_type == "main_league") {
    
    stop("must suppy season_ids for main leagues")
    
  }

  # Downloading a main league

  if(data_type == "main_league"){

    # Format the urls, files, etc

    seasons_short <- paste0(str_sub(season_ids, 3, 4), str_sub(season_ids, 8, 9))

    results_urls <- paste0("https://www.football-data.co.uk/mmz4281/" , seasons_short, "/", 
                           metadata_football_data$url_suffix, ".csv")

    results_filenames <- paste0(path, competition_id, "_", season_ids, ".csv")

  } else if(data_type == "extra_league"){
    
    results_urls <- metadata$football_data$url_full
    results_filenames <- paste0(path, competition_id, "_all", ".csv")
  } else {
    
    stop("unknown data_type check metadata")

  }

  # Download files

  tryCatch({
    
    walk2(results_urls, results_filenames, download.file)

  }, warning = function(warning_condition) {
    
    message(warning_condition)
    return(NA)

  }, error = function(error_condition) {
    
    message(error_condition)
    return(NA)
  })

  return(NA)
}


