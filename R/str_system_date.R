#' Function to get system time with sytem calls. 
#' 
#' @param tz Should the time-zone be included? Default is \code{TRUE}.
#' 
#' @author Stuart K. Grange
#' 
#' @export
str_system_date <- function(tz = TRUE) {
  
  if (.Platform$OS.type == "windows") {
    
    # Use R for Windows
    date <- as.character(Sys.time())
    
  } else {
    
    # Get system date
    if (tz) {
      
      date <- system("date '+%Y-%m-%d %H:%M:%S.%3N %Z'", intern = TRUE)
      
    } else {
      
      date <- system("date '+%Y-%m-%d %H:%M:%S.%3N'", intern = TRUE)
      
    }
    
  }
  
  # Return
  date
  
}
