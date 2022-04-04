#' Functions for calculating speed in m s-1 and pace in min km-1. 
#' 
#' @param seconds Numeric vector of time in seconds. 
#' 
#' @param metres Numeric vector of metres covered within the time period 
#' in \code{seconds}.
#' 
#' @param round For \code{calculate_pace}, the number of digits to round the 
#' output too. 
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
#' # Calculate pace of a run, the input takes seconds and metres
#' # Use seconds for time
#' calculate_pace(5400, half_marathon() * 1000)
#' 
#' # Or the hms data type
#' calculate_pace(parse_time("01:30:00"), half_marathon() * 1000)
#' 
#' @export
calculate_speed <- function(seconds, metres) metres / seconds


#' @rdname calculate_speed
#' @export
calculate_pace <- function(seconds, metres, round = NA) {
  
  # Push hms to seconds if needed
  if (hms::is_hms(seconds)) seconds <- as.numeric(seconds)
  
  # Calculate speed in m s-1 and convert to min km-1, this will return an hms
  # data type
  x <- calculate_speed(seconds, metres) %>% 
    ms_to_min_km(round = round)
  
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
  
  .Deprecated(
    msg = "`calculate_target_pace` is deprecated, please use `threadr::calculate_pace`."
  )
  
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
