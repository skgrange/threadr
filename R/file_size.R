#' Function to get file size. 
#' 
#' @author Stuart K. Grange
#' 
#' @param file File name. 
#' @param unit Measurement unit. Default is \code{"mb"} for megabytes. 
#' 
#' @export
file_size <- function(file, unit = "mb") {
  
  # Get size in bytes
  x <- file.size(file)
  
  if (unit == "mb") {
    
    x <- x / 1000000  # 1048576
    x <- round(x, 2)
    
  }
  
  # Return
  x
  
}



#' Function to get the file size of a SQLite database. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con SQLite database connection. 
#' @param unit Measurement unit. Default is \code{"mb"} for megabytes. 
#' 
#' @export
db_size <- function(con, unit = "mb") {
  
  # Only SQlite
  if (grepl("sqlite", class(con)[1], ignore.case = TRUE)) {
    
    file_name <- con@dbname
    x <- file_size(file_name, unit = unit)
    
  }
  
  # Return
  x
  
}
