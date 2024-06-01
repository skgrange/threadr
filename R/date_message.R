#' Functions to get a formatted date string, usually used as prefixes to 
#' messages.
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
#' @export
date_message <- function() stringr::str_c(str_date_formatted(), ": ")


#' @rdname date_message
#' @export
cli_date <- function() stringr::str_c(str_date_formatted(), ":")


#' @rdname date_message
#' @export
message_date_prefix <- function() {
  
  .Deprecated("date_message", package = "threadr")
  stringr::str_c(str_date_formatted(), ": ")
  
}


#' Function to format a date (usually) for printing. 
#' 
#' @param date Optional, a date to be formatted. 
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


#' Function to print the length of an object, generally for printing with 
#' \strong{cli} messaging. 
#' 
#' \code{cli_n} will return the number of rows of a data frame, not the number 
#' of columns. 
#' 
#' @author Stuart K. Grange
#' 
#' @param x Object to print length of. 
#' 
#' @param sep Should the length be formatted with a thousands separator? 
#' 
#' @export
cli_n <- function(x, sep = FALSE) {
  
  # Get length of object
  if (!is.data.frame(x)) {
    n <- length(n)
  } else {
    n <- nrow(x)
  }
  
  # Add thousands separator
  if (sep) {
    n <- format(n, big.mark = " ", scientific = FALSE)
  }
  
  return(n)
  
}
