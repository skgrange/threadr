#' Function to calculate deltas or differences between two vectors. 
#' 
#' @param x,y Numeric vectors. 
#' 
#' @param absolute Should deltas be represented absolutely (all positive)? 
#' 
#' @param round Number of digits to round deltas to. 
#'
#' @author Stuart K. Grange. 
#' 
#' @return Numeric vector
#' 
#' @examples 
#' 
#' # Calculate errors 
#' calculate_deltas(sample(1:5), sample(1:5))
#' 
#' # Absolute
#' calculate_deltas(sample(1:5), sample(1:5), absolute = TRUE)
#'
#' @export
calculate_deltas <- function(x, y, absolute = FALSE, round = NA) {
  
  # Calculate delta
  delta <- x - y
  
  # Make absolute
  if (absolute) delta <- abs(delta)
  
  # Round
  if (!is.na(round[1])) delta <- round(delta, digits = round)
  
  return(delta)
  
}
