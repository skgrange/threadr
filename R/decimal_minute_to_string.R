#' Function to transform decimal minutes into a formatted \code{"HH:MM:SS"} or
#' \code{"MM:SS"} string.
#' 
#' @param x Numeric vector of decimal minutes.
#'  
#' @param hour Should the hour also be included in the formatted string? Default
#' is \code{TRUE}.
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' 
#' decimal_minute_to_string(5)
#' 
#' decimal_minute_to_string(5.5)
#' 
#' decimal_minute_to_string(5.34)
#' 
#' decimal_minute_to_string(5.954226)
#' 
#' # Without the hour piece
#' decimal_minute_to_string(5.954226, hour = FALSE)
#' 
#' }
#' 
#' @export
decimal_minute_to_string <- function(x, hour = TRUE) {
  
  # Check
  stopifnot(is.numeric(x))
  
  # Format
  x <- lubridate::duration(x, units = "minutes")
  x <- lubridate::as.period(x)
  x <- lubridate::period_to_seconds(x)
  x <- parse_unix_time(x)
  
  if (hour) {
    
    x <- format(x, format = "%H:%M:%OS")
    
  } else {
    
    x <- format(x, format = "%M:%OS")
    
  }
  
  return(x)
  
}
