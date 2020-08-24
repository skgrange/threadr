#' Function to convert speed in m.s-1 to minutes per kilometre. 
#' 
#' @param x Speed in metres per second (m.s-1). 
#' 
#' @param round Number of digits to round the return to. 
#' 
#' @return \strong{hms} vector. 
#' 
#' @export
ms_to_min_km <- function(x, round = NA) {
  
  # To minutes per km-1
  x <- 60 / (x * 3.6)
  
  # Calculate the two components
  minutes <- floor(x)
  fractional_minute <- x %% minutes
  
  # Transform the components
  minutes_as_seconds <- minutes * 60
  fractional_seconds <- fractional_minute * 60
  
  # Sum
  x <- minutes_as_seconds + fractional_seconds
  
  # Round if desired
  if (!is.na(round)) x <- round(x, round)
  
  # To hms data type
  x <- hms::as_hms(x)
  
  return(x)
  
}
