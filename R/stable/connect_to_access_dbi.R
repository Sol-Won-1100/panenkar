
#' Connect to Access DBI
#' 
#' Connect to access via a DBI connector
#' 
#' @param file
#' @return connection to your access database
#' @export

connect_to_access_dbi <- function(file)  {
  
  # make sure that the file exists before attempting to connect
  
  if (!file.exists(file)) {
    
    stop("file does not exist at ", file)
    
  }
  
  if (str_ends(".accdb", negate = TRUE)) {
    
    stop("bad file extension must be .accdb")
    
  }  
  
  # Assemble connection strings
  
  dbq_string <- paste0("DBQ=", file)
  driver_string <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
  db_connect_string <- paste0(driver_string, dbq_string)
  
  db_connection <- dbConnect(odbc::odbc(), .connection_string = db_connect_string)

  return(db_connection)
}