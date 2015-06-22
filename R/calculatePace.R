#' Function for calculating pace, usually in min km-1
#' 
#' 
#' @author Stuart Grange
#'
#' @export
#'
#'
#'
#'
calculatePace <- function(time = time, distance = distance){
  
  # Pace minute
  pace.minute <- time / distance / 60
  pace.minute <- floor(pace.minute)
  
  # Pace seconds
  pace.seconds <- round((time / distance) %% 60, 0)
  # Pad with a zero if needed
  pace.seconds <- stringr::str_pad(pace.seconds, width = 2, pad = '0')
  
  # Combine minute and seconds
  pace <- stringr::str_c(pace.minute, pace.seconds, sep = '.')
  # Type convert
  pace <- as.numeric(pace)
  
  # Return
  pace
}
