#' Function to format a date in a verbose, English language style. 
#' 
#' @param x Date vector. 
#' 
#' @param weekday Should the weekday be returned? 
#' 
#' @param seconds Should seconds be printed? 
#' 
#' @param time_zone Should the time zone string be printed? 
#' 
#' @return Character vector. 
#' 
#' @author Stuart K. Grange.
#' 
#' @examples 
#' 
#' # All information with seconds and time zone
#' str_english_date_format(lubridate::now())
#' 
#' # No seconds
#' str_english_date_format(lubridate::now(), seconds = FALSE)
#' 
#' # No seconds or time zone
#' str_english_date_format(lubridate::now(), seconds = FALSE, time_zone = FALSE)
#' 
#' @export 
str_english_date_format <- function(x, weekday = TRUE, seconds = TRUE, 
                                    time_zone = TRUE) {
  
  # The formatting string
  format_string <- "%A, %B %d %Y %H:%M"
  
  # Drop weekday
  if (!weekday) format_string <- stringr::str_remove(format_string, "%A, ")
  
  # Append seconds
  if (seconds) format_string <- stringr::str_c(format_string, ":%OS3")
  
  # To-do: fractional second logic
  
  # Do the formating
  x <- format(x, format_string, usetz = time_zone)
  
  return(x)
  
}
