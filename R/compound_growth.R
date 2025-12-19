#' Function to calculate compound growth. 
#' 
#' @param start The starting condition where compound growth begins. 
#' 
#' @param rate The growth rate. 
#' 
#' @param steps The number of discrete growth iterations. 
#' 
#' @return Numeric vector.
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' 
#' # Start at 15 and grow by 10 % ten times
#' compound_growth(start = 15, rate = 0.1, steps = 10)
#' 
#' # Start at 15 and grow by 50 % ten times
#' compound_growth(start = 15, rate = 0.5, steps = 10)
#' 
#' @export
compound_growth <- function(start, rate, steps = 1) {
  # The growth is based on the previous value
  start * (1 + rate) ^ (seq_len(steps) - 1)
}
