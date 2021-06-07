#' Function to convert speed in meters per second (m.s-1) to minutes per 
#' kilometre (min.km-1). 
#' 
#' @param x Speed in metres per second (m.s-1). 
#' 
#' @param round Number of decimal points to round the return to.
#' 
#' @return \strong{hms} vector. 
#' 
#' @export
ms_to_min_km <- function(x, round = NA) {
  
  # Conversion factor for m.s-1 to min.km-1
  coefficient <- 1/3.6 * 60
  
  # Convert to from m.s-1 to km.m-1
  x <- x / coefficient
  
  # Invert to get min.km-1 and push into seconds
  x <- 1 / x * 60
  
  # Round if desired
  if (!is.na(round)) x <- round(x, round)
  
  # Make hms data type
  x <- hms::as_hms(x)
  
  return(x)
  
}
