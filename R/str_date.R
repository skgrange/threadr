#' Function to return the system's idea of the date as a character string. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
#' 
str_date <- function (time = TRUE, tz = TRUE, underscore = FALSE) {
  
  # tz argument is redundant if time is set to FALSE
  tz <- ifelse(!time, FALSE, tz)
  
  # If timezone information is desired, then time is needed too
  if (time | tz) {

    # Get date with time
    date <- as.character(Sys.time())
    
  } else {
    
    # Just the date
    date <- as.character(Sys.Date())
    
  }
  
  if (tz) {
    
    # Get time zone
    # To-do: do a unix work-around, returning NA if zone is not set
    time.zone <- Sys.timezone(location = TRUE)
    
	# Add time zone to string
    if (!is.na(time.zone)) {
      date <- paste(date, time.zone)
    }
  
  }
  
  # Useful for file names
  if (underscore) {
    date <- stringr::str_replace_all(date, " |:|-|/", "_")
  }
  
  # Return 
  date
  
}
