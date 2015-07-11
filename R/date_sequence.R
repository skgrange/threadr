#' Function to generate a sequence of past or future dates. 
#' 
#' date_sequence is useful for generating patterns which match file-names.
#' 
#' @param days Number of days of the sequence from system's date. Use negative
#' numbers to go back in time. 
#' @param as.character. Should the date sequence be returned as a character 
#' vector rather than a date vector? Default is TRUE. 
#' @param today Should system date be included in the returned sequence? 
#' 
#' @author Stuart K. Grange
#'
#' @examples
#' # Make a date sequence of the last three days and today
#' pattern <- date_sequence(days = -3)
#'
#' # Display
#' pattern
#' 
#' "2015-06-22" "2015-06-23" "2015-06-24" "2015-06-25"
#'
#' @export
#' 
date_sequence <- function (days = -1, as.character = TRUE, today = TRUE) {
  
  # Get system date
  date.system <- lubridate::ymd(Sys.Date())
  
  if (days < 0) {
    
    # Get past dates
    date.sequence <- seq(date.system + lubridate::days(days), date.system, 
                         by = "days")
    
    
  } else {
    
    # Get future dates
    date.sequence <- seq(date.system, date.system + lubridate::days(days),
                         by = "days")
    
  }
  
  # Remove today's date
  if (!today) {
    date.sequence <- date.sequence[!date.sequence %in% date.system]
  }
  
  # Make a character
  if (as.character) {
    date.sequence <- as.character(date.sequence)
  }
  
  # Return
  date.sequence
  
}
