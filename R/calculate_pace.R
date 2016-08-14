#' Function for calculating pace in min km-1. 
#' 
#' @param seconds Numeric vector of time in seconds. 
#' 
#' @param metres Numeric vector of metres covered within the time period 
#' in \code{seconds}.
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
#' df$pace <- calculate_pace(df$seconds, df$distance)
#' 
#' }
#' 
#' @export
calculate_pace <- function(seconds, metres) {
  
  # Calculate speed in m s-1
  x <- metres / seconds
  
  # Transform to km h-1
  x <- ms_to_km_h(x)
  
  # Then to pace
  x <- km_h_to_min_km(x)
  
  # Return
  x
  
}
