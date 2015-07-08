#' Function to round dates to arbitrary time intervals such as 5-, 10-, 15-, or
#' 30-minute periods.
#'
#' round_date_interval rounds dates to the nearest arbitrary time interval 
#' unlike xts::align.time which always pushes dates to the next interval. Dates 
#' that fall in the centre of the time interval are rounded down. 
#' 
#' round_date_interval works by first calculating an origin which is the first 
#' instant of the date's hour. round_date_interval then extracts the 
#' seconds-past-the-origin of the date and rounds this value to the interval's 
#' multiple. The rounded value and the origin are then summed and returned.
#' 
#' round_date_interval does not make any adjustments for duplicated dates in the
#' input or output.
#' 
#' round_date_interval can be used for rounding dates to the nearest whole
#' minute or hour, however, lubridate::round_date and related functions may be 
#' more convenient. 
#' 
#' Currently round_date_interval cannot deal with multiples other than minutes.
#' Intervals such as 2 hours will not work yet. 
#' 
#' The \code{interval} argument can take 'friendly' forms such as 
#' \code{'5 mins'} or \code{'15 min'} as well as \code{5} or \code{15}.
#'
#' @param date A vector of dates to be rounded. This variable must be named 
#' \code{'date'}.
#' @param interval A numeric or string value which represents the time interval
#' in minutes. 
#' 
#' @author Stuart K. Grange
#' 
#' @seealso See \code{\link{time_pad}}, \code{\link{lubridate::round_date}}
#' 
#' @examples
#' 
#' \dontrun{
#" # A messy date vector
#" date.vector <- c("2015-04-09 06:25:23", "2015-04-09 06:41:27",
#"                  "2015-04-09 07:02:27", "2015-04-09 07:50:40")
#" 
#" # Parse dates
#" date.vector <- lubridate::ymd_hms(date.vector)
#" 
#" # Round dates to 15 minute intervals
#" round_date_interval(date.vector, "15 min")
#" "2015-04-09 06:30:00 UTC" "2015-04-09 06:45:00 UTC" "2015-04-09 07:00:00 UTC"
#" "2015-04-09 07:45:00 UTC"
#" 
#" # Round dates to 5 minute intervals
#" round_date_interval(date.vector, "5 mins")
#" "2015-04-09 06:25:00 UTC" "2015-04-09 06:40:00 UTC" "2015-04-09 07:00:00 UTC"
#" "2015-04-09 07:50:00 UTC"
#' }
#' 
#' @export
#' 
round_date_interval <- function (date, interval = "5 min") {
  
  # Parse argument into numeric value
  # To-do: use switch
  # Extract number-characters in string
  interval <- stringr::str_replace_all(interval, "([0-9]+).*$", "\\1")
  # Convert to numeric
  interval <- type.convert(interval)
  
  # Floor round date to hour, 0 minutes and seconds, an origin
  date.floor <- lubridate::floor_date(date, "hour")
  
  # Pull out the pieces of the date
  # Minutes
  minutes <- lubridate::minute(date)
  minutes.in.seconds <- minutes * 60
  
  # Seconds
  seconds <- lubridate::second(date)
  
  # Add minutes and seconds of date
  seconds.sum <- minutes.in.seconds + seconds
  
  # Round to nearest interval multiple
  second.rounded <- (60 * interval) * round(seconds.sum / (60 * interval))
  
  # Add rounded seconds to floor rounded date
  date <- date.floor + lubridate::seconds(second.rounded)
  
  # Return
  date
  
}
