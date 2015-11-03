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
#' \code{unix_time} has an argument (\code{tzone}) where the correct time-zone 
#' for the date can be supplied only for this function. 
#' 
#' \code{OlsonNames()} lists the time zones which can be used. 
#' 
#' @param date Date to be transformed into unix time. 
#' @param tz Olson time-zone string for \code{date} if \code{date} has incorrect
#' time-zone information. 
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#'
#' \dontrun{
#' # Add unix time to a data frame which contains local dates
#' data_air_quality$unix_time <- unix_time(data_air_quality$date, 
#'   tz = "Pacific/Auckland")
#'
#' }
#' 
#' @export
unix_time <- function (date, tz = NA) {
  
  # Overwrite time zone info. Same clock/values, different moment
  if (!is.na(tz)) {
    date <- lubridate::force_tz(date, tz)
    
  }

  # Convert date to numeric, i.e. unix time
  date <- as.numeric(date)
  
  # Return
  date
  
}
