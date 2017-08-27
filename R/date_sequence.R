#' Function to generate a sequence of past or future dates. 
#' 
#' \code{date_sequence} is useful for generating patterns which match file names.
#' 
#' @param days Number of days of the sequence from system's date. Use negative
#' integers to go back in time. 
#'
#' @param character Should the date sequence be returned as a character vector 
#' rather than a date vector? Default is \code{TRUE}. 
#'
#' @param today Should system date be included in the returned sequence? 
#'
#' @param sep What seperator should be used between the year, month, and day
#' pieces of the date? If this is changed from \code{"-"} (hyphen), then the 
#' returned vector will always be a character.
#' 
#' @author Stuart K. Grange
#'
#' @examples
#' \dontrun{
#' 
#' # Make a date sequence of the last three days and today
#' pattern <- date_sequence(days = -3)
#' 
#' }
#'
#' @export
date_sequence <- function(days = -1, character = TRUE, today = TRUE, sep = "-") {
  
  # If character is not a hyphen, then the dates will always be characters
  if (sep != "-") character <- TRUE
  
  # Get system date
  date_system <- lubridate::ymd(Sys.Date(), tz = "UTC")
  
  if (days < 0) {
    
    # Get past dates
    date_sequence <- seq(
      date_system + lubridate::days(days), 
      date_system, 
      by = "days"
    )
    
  } else {
    
    # Get future dates
    date_sequence <- seq(
      date_system, 
      date_system + lubridate::days(days),
      by = "days"
    )
    
  }
  
  # Remove today's date
  if (!today) date_sequence <- date_sequence[!date_sequence %in% date_system]
  
  # Make a character
  if (character) date_sequence <- as.character(date_sequence)
  
  if (sep != "-") 
    date_sequence <- stringr::str_replace_all(date_sequence, "-", sep)
  
  # Return
  date_sequence
  
}
