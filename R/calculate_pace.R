#' Function for calculating pace in min km-1. 
#' 
#' @param time Numeric vector of time in seconds. 
#' 
#' @param distance Numeric vector of kilometers covered within the time period 
#' in \code{time}.
#' 
#' @return Numeric vector of pace in min km-1.
#' 
#' @keywords pace, running, sport, cycling
#' 
#' @author Stuart K. Grange
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' # Trasform time and distance to pace in a data frame
#' df$pace <- calculate_pace(time, distance)
#' 
#' }
#' 
calculate_pace <- function(time, distance) {
  
  # Pace minute
  pace_minute <- time / distance / 60
  pace_minute <- floor(pace_minute)
  
  # Pace seconds
  pace_seconds <- round((time / distance) %% 60, 0)
  
  # Pad with a zero if needed
  pace_seconds <- stringr::str_pad(pace_seconds, width = 2, pad = "0")
  
  # Combine minute and seconds
  pace <- stringr::str_c(pace_minute, pace_seconds, sep = ".")
  
  # Type convert
  pace <- as.numeric(pace)
  
  # Return
  pace
  
}
