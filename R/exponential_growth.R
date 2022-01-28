#' Function to calculate exponential growth. 
#' 
#' Exponential growth is also called geometric growth and has the property of 
#' the rate of change is proportional to the quantity itself. 
#' 
#' @param start The starting condition where exponential growth begins. 
#' 
#' @param rate The growth rate or exponent to represent growth. 
#' 
#' @param steps The number of discrete growth iterations. 
#' 
#' @return Numeric vector.
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' 
#' # Starting condition of 10000 with a low value for the exponential growth
#' exponential_growth(start = 10000, rate = 1.1, steps = 6)
#' 
#' # The same starting condition but with twice the growth rate, a huge change
#' exponential_growth(start = 10000, rate = 1.1 * 2, steps = 6)
#' 
#' @export
exponential_growth <- function(start, rate, steps = 1) {
  
  # Make sure the number of steps is an integer
  steps <- as.integer(steps)
  
  # Pre-allocate vector
  x <- numeric(length = steps)
  
  # To the multiplication, the growth is based on it's previous value and
  # therefore, a for loop needs to be used
  for (i in seq_len(steps)) {
    if (i == 1L) {
      x[i] <- start
    } else {
      x[i] <- x[i - 1L] * rate
    }
  }
  
  return(x)
  
}
