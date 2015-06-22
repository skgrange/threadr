#' Function to calculate unix time from a date vector.
#' 
#' unixTime returns the numeric value from a date which represents seconds 
#' since the unix epoch (1970-01-01 (UTC); excluding leap seconds). unixTime
#' can be very useful to store because it is time zone independent and is 
#' efficent to parse. 
#' 
#' Dates in R are often parsed and interacted without including correct time 
#' zone information. However, if the time zone information for an ipnut date is 
#' incorrect, the return of unixTime will also be incorrect. Rather than forcing
#' users to correctly parse all dates, unixTime has an argumment 
#' \code{tz.overwrite} where the correct time zone for the date can be supplied 
#' only for this function. \code{OlsonNames()} lists the time zones which can be
#' used. 
#' 
#' @author Stuart K. Grange
#' 
unixTime <- function (date, tz.overwrite = NULL) {
  
  # Remind the user that time zone info must be correct
  message('Ensure the time zone of your date is correct.')
  
  # Overwrite time zone info. Same clock/values, different moment
  if (!is.null(tz.overwrite)) {
    
    date <- lubridate::force_tz(date, tz.overwrite)
    
  }
  
  # Make this date UTC, the date in the UTC time zone
  date <- lubridate::with_tz(date, 'UTC')
  
  # Convert date to numeric, i.e. unix time
  date <- as.numeric(date)
  
  # Return
  date
  
}