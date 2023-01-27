#' Function to test elements of a vector are within a given quantile. 
#' 
#' @author Stuart K. Grange
#' 
#' @param x A numeric vector.  
#' 
#' @param probabilities A numeric vector with the length of \code{2} to use as 
#' the quantile limits. 
#' 
#' @param type Method to use to calculate the quantiles. 
#' 
#' @param na.rm Should \code{NA} values be omitted? The \code{\link{quantile}} 
#' function cannot be applied to vectors with \code{NA} values. 
#' 
#' @return Logical vector with length of \code{x}.  
#' 
#' @seealso \code{\link{quantile}}, \code{\link{calculate_quantiles}}
#' 
#' @export
is_outside_quantiles <- function(x, probabilities = c(0.01, 0.99), type = 7,
                                 na.rm = TRUE) {
  
  # Check inputs
  stopifnot(length(probabilities) == 2L)
  
  # Calculate the quantile limits
  quantiles <- unname(quantile(x, probs = probabilities, na.rm = na.rm))
  
  # Test if vector is within the limits
  x <- x <= quantiles[1] | x >= quantiles[2]
  
  return(x)
  
}
