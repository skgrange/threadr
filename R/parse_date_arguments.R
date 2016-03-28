#' Function to parse date arguments for other functions. 
#' 
#' @param date Date string to be parsed. 
#' @param type Type of date: \code{"start"} or \code{"end"}. 
#' 
#' @author Stuart K. Grrange
#'
#' @export 
parse_date_arguments <- function(date, type) {
  
  # If no date used just used system date, deos not matter what type
  if (is.na(date)) {
    
    date <- lubridate::ymd(Sys.Date())
    
  } else {
    
    # Start of year
    if (type == "start") {
      
      # Catch for when years are used as dates
      if (stringr::str_count(date) == 4) date <- stringr::str_c(date, "-01-01")
      
      # Round
      date <- ifelse(is.na(date), 
                     as.character(lubridate::floor_date(Sys.Date(), "year")), date)
      
    }
    
    # End of year
    if (type == "end") {
      
      if (stringr::str_count(date) == 4) date <- stringr::str_c(date, "-12-31")
      
      # Round
      date <- ifelse(
        is.na(date), as.character(lubridate::ceiling_date(Sys.Date(), "year")), date)
      
    }
    
    # Parse date
    date <- lubridate::parse_date_time(date, c("ymd", "dmy"))
    
  }
  
  # Return
  date
  
}
