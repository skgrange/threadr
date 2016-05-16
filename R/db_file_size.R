#' Function to get the file size of a SQLite database. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con SQLite database connection. 
#' @param unit Measurement unit. Default is \code{"mb"} for megabytes. 
#' 
#' @export
db_file_size <- function(con, unit = "mb") {
  
  # Only SQlite
  if (grepl("sqlite", class(con)[1], ignore.case = TRUE)) {
    
    file_name <- con@dbname
    x <- file_size(file_name, unit = unit)
    
  }
  
  # Return
  x
  
}
