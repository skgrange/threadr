#' Function to calculate mode (the value that appears most often) of a vector.
#' 
#' @author Ken Williams. 
#' 
#' @param x Input vector.
#' 
#' @param na.rm Should \code{NA}s be omitted?
#' 
#' @return Vector with a length of \code{1}. 
#' 
#' @seealso \href{http://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode}{stackoverflow},
#' \code{\link{mean}}, \code{\link{median}}
#' 
#' @examples
#' 
#' # Calculate the mode of integers
#' mode_average(c(1L, 2L, 3L, 3L))
#' 
#' # Calculate the mode of a numeric vector
#' mode_average(c(5, 6, 6, 500000))
#' 
#' # Will return the first element when one or two elements are supplied
#' mode_average(c(5, 1))
#' mode_average(c("hail", "rain"))
#' 
#' # Will also work with character and factor vectors
#' mode_average(c("rain", "hail", "sunshine", "rain"))
#' 
#' @export
mode_average <- function(x, na.rm = FALSE) {
  
  # Drop missing elements
  if (na.rm) x <- na.omit(x)
  
  # Compute the mode of the vector
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
  
}
