#' Function to get file size. 
#' 
#' @author Stuart K. Grange
#' 
#' @param file File name. 
#' 
#' @param unit Measurement unit. Default is \code{"mb"} for megabytes. 
#' 
#' @return Numeric vector with length of one. 
#' 
#' @export
file_size <- function(file, unit = "mb") {
  
  # Get size in bytes
  x <- file.size(file)
  
  if (unit == "mb") {
    
    x <- x / 1000000  # or 1048576?
    x <- round(x, 2)
    
  }
  
  return(x)
  
}
