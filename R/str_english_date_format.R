#' Function to format a date in a verbose, English language character string. 
#' 
#' @param x Date vector. 
#' 
#' @param weekday Should the weekday be returned? 
#' 
#' @param seconds Should seconds be printed? 
#' 
#' @param time_zone Should the time zone string be printed? 
#' 
#' @param date_only Should only the date pieces be returned? 
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
                                    time_zone = TRUE, date_only = FALSE) {
  
  # The formatting string
  format_string <- "%A, %B %d %Y %H:%M"
  
  # Just the date
  if (date_only) {
    
    time_zone <- FALSE
    format_string <- stringr::str_remove(format_string, " %H:%M")
    
  } else {
    
    # Append seconds
    if (seconds) format_string <- stringr::str_c(format_string, ":%OS3")
    # To-do: fractional second logic
    
  }
  
  # Drop weekday
  if (!weekday) format_string <- stringr::str_remove(format_string, "%A, ")
  
  # Do the formating
  x <- format(x, format_string, usetz = time_zone)
  
  # Remove leading zeros
  x <- stringr::str_replace_all(x, " 0", " ")
  
  return(x)
  
}
