#' Function to calculate unix time from a date vector.
#' 
#' \code{unix_time} returns the numeric value from a date which represents 
#' seconds since the unix epoch (1970-01-01 (UTC); excluding leap seconds). 
#' \code{unix_time} can be very useful to have because it is time-zone 
#' independent and is efficient to parse and store. 
#' 
#' Dates in R are often parsed and interacted with without including correct 
#' time-zone information for convenience. However, if the time-zone information 
#' for an input date is incorrect, the return of \code{unix_time} will also be 
#' incorrect. Rather than forcing users to correctly parse all dates, 
#' \code{unix_time} has an argument (\code{tz}) where the correct time-zone for
#' the date can be supplied only for this function. 
#' 
#' \code{OlsonNames()} lists the time zones which can be used. 
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' # Add unix time to a data frame which contains local dates
#' data.air.quality$unix.time <- unix_time(data.air.quality$date, 
#'   tz.overwrite = "Pacific/Auckland")
#' }
#' 
#' @export
#' 
unix_time <- function (date, tz = NA) {
  
  # Remind the user that time zone info must be correct
  message("Ensure the time-zone of your date is correct.")
  
  # Overwrite time zone info. Same clock/values, different moment
  if (!is.na(tz)) {
    date <- lubridate::force_tz(date, tz)
  }
  
  # Make this date UTC, the date in the UTC time zone
  date <- lubridate::with_tz(date, "UTC")
  
  # Convert date to numeric, i.e. unix time
  date <- as.numeric(date)
  
  # Return
  date
  
}
