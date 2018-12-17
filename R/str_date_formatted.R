#' Function to format a date (usually) for printing. 
#' 
#' @param date Optional, a date to be formated. 
#' 
#' @param time_zone Should the time zone be printed with the date? 
#' 
#' @param fractional_seconds Should fractional seconds be included when printing
#' the date? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Character vector. 
#' 
#' @export
str_date_formatted <- function(date = NA, time_zone = TRUE, 
                               fractional_seconds = TRUE) {
  
  # Get date if not supplied
  if (is.na(date)[1]) date <- lubridate::now(tz = Sys.timezone())
  
  # Format string
  format_date <- ifelse(
    fractional_seconds, 
    "%Y-%m-%d %H:%M:%OS3", 
    "%Y-%m-%d %H:%M:%S"
  )
  
  # Format
  x <- format(date, format = format_date, usetz = time_zone)
  
  return(x)
  
}


#' Function to get a formatted date string used to prefix messages. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Character vector.
#' 
#' @examples 
#' 
#' # The message prefix
#' date_message()
#' 
#' # Within a message
#' message(date_message(), "Loading...")
#' 
#' @rdname str_date_formatted
#' 
#' @export
date_message <- function() stringr::str_c(str_date_formatted(), ": ")


#' @rdname str_date_formatted
#' 
#' @export
message_date_prefix <- function() {
  
  .Deprecated("date_message", package = "threadr")
  stringr::str_c(str_date_formatted(), ": ")
  
}
