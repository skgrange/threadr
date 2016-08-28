#' Functions for calculating speed in m s-1 and pace in min km-1. 
#' 
#' @param seconds Numeric vector of time in seconds. 
#' 
#' @param metres Numeric vector of metres covered within the time period 
#' in \code{seconds}.
#' 
#' @return Numeric vector of speed in m s-1 or pace in min km-1. 
#' 
#' @keywords speed, pace, running, sport, cycling
#' 
#' @author Stuart K. Grange
#' 
#' @examples
#' 
#' \dontrun{
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
#' }
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
  
  # Return
  x
  
}
