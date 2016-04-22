#' Function to parse date arguments for other functions. 
#' 
#' @param date Date string to be parsed. 
#' @param type Type of date: \code{"start"} or \code{"end"}. 
#' @param tz Time-zone, defaults to \code{"UTC"}. 
#' 
#' @author Stuart K. Grrange
#'
#' @export 
parse_date_arguments <- function(date, type, tz = "UTC") {
  
  # If no date used just used system date, does not matter what type
  if (is.na(date)) {
    
    date <- lubridate::ymd(Sys.Date(), tz = tz)
    
  } else {
    
    # Get system date for future rounding
    date_system <- lubridate::ymd(Sys.Date(), tz = tz)
    
    if (type == "start") {
      
      # Catch for when years are used as dates
      if (stringr::str_count(date) == 4) date <- stringr::str_c(date, "-01-01")

      # Round to start of year
      date <- ifelse(
        is.na(date), as.character(lubridate::floor_date(date_system, "year")), date)
      
    }
    
    if (type == "end") {
      
      # Catch for when years are used as dates
      if (stringr::str_count(date) == 4) date <- stringr::str_c(date, "-12-31")
      
      # Round to end of year
      date <- ifelse(
        is.na(date), as.character(lubridate::ceiling_date(date_system, "year")), date)
      
    }
    
    # Parse date
    date <- lubridate::parse_date_time(date, c("ymd", "dmy"), tz = tz)
    
  }
  
  # Return
  date
  
}
