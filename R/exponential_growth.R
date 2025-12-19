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
#' # The same starting condition but with twice the growth rate, a big change
#' exponential_growth(start = 10000, rate = 1.1 * 2, steps = 6)
#' 
#' @export
exponential_growth <- function(start, rate, steps = 1) {
  # The growth is based on the previous value
  start * rate^(seq_len(steps) - 1)
}
