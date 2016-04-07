#' Function to extract time-zone from a POSIXct date.  
#' 
#' @author Stuart K. Grange
#' 
#' @param date Date vector
#' 
#' @return Vector with a length of one. 
#' 
#' @examples 
#' \dontrun{
#' 
#' # A date vector
#' time_zone(date_parsed)
#' 
#' # A variable in data frame
#' time_zone(data_test$date)
#' 
#' }
#' 
#' @export
time_zone <- function(date) attr(date, "tzone")


#' Function to conveniently parse a vector of unix time to a POSIXct date vector. 
#' 
#' @author Stuart K. Grange
#' 
#' @param x An integer- or numeric-vector of unix times. 
#' 
#' @param tz Time-zone string. \code{parse_unix_time} defaults to \code{"UTC"}. 
#' 
#' @param origin Origin of epoch. By definition, unix time is \code{"1970-01-01"},
#' but other epochs are in use. 
#' 
#' @return A POSIXct vector with the length of x. 
#' 
#' @examples
#' \dontrun{
#' 
#' # A vector
#' unix_time_vector <- c(1460034000, 1460034703)
#' 
#' # Parse time, will be in UTC time-zone
#' parse_unix_time(unix_time_vector)
#' 
#' # Or in Berlin's time-zone
#' parse_unix_time(unix_time_vector, tz = "Europe/Berlin")
#' 
#' }
#' 
#' @export 
parse_unix_time <- function(x, tz = "UTC", origin = "1970-01-01") {
  
  # A switch for my common usage
  if (tz == "nz") tz <- "Etc/GMT-12"
  
  # Parse/promote
  x <- as.POSIXct(x, tz = tz, origin = origin)
  
  # Return
  x
  
}
