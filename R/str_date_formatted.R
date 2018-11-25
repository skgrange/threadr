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
#' @return Character vector
#' 
#' @examples 
#' 
#' # The message prefix
#' message_date_prefix()
#' 
#' # Within a message
#' message(message_date_prefix(), "Loading...")
#' 
#' @export
message_date_prefix <- function() stringr::str_c(str_date_formatted(), ": ")
