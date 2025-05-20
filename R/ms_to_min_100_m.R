#' Function to convert speed in meters per second (m.s-1) to minutes per 
#' 100 metres (min.100.m-1). 
#' 
#' @param x Speed in metres per second (m.s-1). 
#' 
#' @param round Number of decimal points to round the return to. 
#' 
#' @return \strong{hms} vector. 
#' 
#' @export
ms_to_min_100_m <- function(x, round = NA) {
  
  # To metres per minute
  x <- x * 60
  
  # To seconds per 100 
  x <- (100 / x) * 60
  
  # Round if desired
  if (!is.na(round[1])) {
    x <- round(x, digits = round)
  }
  
  # To hms
  x <- hms::as_hms(x)
  
  return(x)
  
}
