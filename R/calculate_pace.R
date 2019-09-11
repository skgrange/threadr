#' Functions for calculating speed in m s-1 and pace in min km-1. 
#' 
#' @param seconds Numeric vector of time in seconds. 
#' 
#' @param metres Numeric vector of metres covered within the time period 
#' in \code{seconds}.
#' 
#' @keywords speed, pace, running, sport, cycling
#' 
#' @seealso \code{\link{decimal_minute_to_string}}
#' 
#' @author Stuart K. Grange
#' 
#' @return Numeric vector of speed in m s-1 or pace in min km-1. 
#' 
#' @examples
#'
#' # Calculate some speeds
#' calculate_speed(9.58, 100)
#' ms_to_km_h(calculate_speed(9.58, 100))
#' 
#' # and pace
#' calculate_pace(19.19, 200)
#' min_km_to_km_h(calculate_pace(19.19, 200))
#' 
#' decimal_minute_to_string(calculate_pace(19.19, 200), hour = FALSE)
#' 
#' calculate_speed(3503, half_marathon() * 1000)
#' decimal_minute_to_string(calculate_pace(3503, half_marathon() * 1000))
#' 
#' @export
calculate_speed <- function(seconds, metres) metres / seconds


#' @rdname calculate_speed
#' @export
calculate_pace <- function(seconds, metres) {
  
  # Calculate speed in m s-1
  x <- calculate_speed(seconds, metres)
  
  # Transform to km h-1
  x <- ms_to_km_h(x)
  
  # Then to pace
  x <- km_h_to_min_km(x)
  
  return(x)
  
}


#' Function to calculate target pace in min km-1. 
#' 
#' @param distance Distance in kilometres. 
#' 
#' @param time A formatted time string such as \code{16:05} or \code{01:24:00}. 
#' 
#' @param as.hms Should the return be of \strong{hms} class? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Character of \strong{hms} vector. 
#' 
#' @export
calculate_target_pace <- function(distance, time, as.hms = FALSE) {
  
  # Pad string
  if (stringr::str_count(time, ":") == 1) time <- stringr::str_c("00:", time)
  
  # Time to seconds
  time <- as.numeric(hms::parse_hms(time))
  
  # Seconds per distance
  x <- time / distance
  
  # Parse
  x <- hms::as_hms(x)
  
  # To character
  if (!as.hms) x <- format(x, format = "%H:%M:%OS")
  
  return(x)
  
}
