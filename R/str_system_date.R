#' Function to get system time with sytem calls. 
#' 
#' @param tz Should the time-zone be included? Default is \code{TRUE}.
#' 
#' @author Stuart K. Grange
#' 
#' @export
str_system_date <- function(tz = TRUE) {
  
  # Soon to be dropped
  .Deprecated(msg = "`str_system_date` is deprecated.")
  
  if (.Platform$OS.type == "windows") {
    
    # Use R for Windows. To-do: fix if someone asks. 
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


#' Function to get system date as an numeric/integer value representing unix 
#' time.
#' 
#' @param integer Should the date be represented as an integer rather than a 
#' numeric value. Default is \code{FALSE}. 
#' 
#' @return Numeric or integer vector with a length of one. 
#' 
#' @author Stuart K. Grange 
#' 
#' @export
sys_unix_time <- function(integer = FALSE) {
  
  # Soon to be dropped
  .Deprecated(msg = "`sys_unix_time` is deprecated.")
  
  # Get system time
  date <- Sys.time()
  
  # To something else
  if (integer) date <- as.integer(date) else date <- as.numeric(date)
  
  # Return
  date
  
}
