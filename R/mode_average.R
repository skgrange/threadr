#' Function to calculate mode of a vector. 
#' 
#' @param x Input vector. 
#' @param na.rm Should \code{NA}s be omitted? Deafult is \code{FALSE}. 
#' 
#' @return Vector with a length of one. 
#' 
#' @seealso \href{http://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode}{stackoverflow},
#' \code{\link{mean}}, \code{\link{median}}
#' 
#' @author Ken Williams with enhancement by Stuart K. Grange
#' 
#' @export
mode_average <- function(x, na.rm = FALSE) {
  
  if (na.rm) x <- na.omit(x)
  
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
  
}
