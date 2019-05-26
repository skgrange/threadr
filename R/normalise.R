#' Function to normalise a numeric vector's range between 0 and 1. 
#' 
#' @param x Numeric vector
#' 
#' @param na.rm Remove NAs? Default is \code{TRUE}.
#' 
#' @return Numeric vector with the length of \code{x}.  
#' 
#' @seealso \href{http://stats.stackexchange.com/questions/70801/how-to-normalize-data-to-0-1-range}{stackexchange}
#' 
#' @author Stuart K. Grange
#' 
#' @export
normalise <- function(x, na.rm = TRUE) {
  
  # Calculate max and min once
  min <- min(x, na.rm = na.rm)
  max <- max(x, na.rm = na.rm)
  
  # Apply normalising function
  x <- (x - min) / (max - min)
  
  return(x)
  
}

