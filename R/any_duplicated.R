#' Function to test if a vector contains duplicate elements. 
#' 
#' Unlike \code{\link{anyDuplicated}} which returns the index value of the first
#' duplicated element, \code{any_duplicated} is simpler and returns a logical
#' vector. 
#' 
#' @author Stuart K. Grange
#' 
#' @param x A vector to test for duplicate elements.
#' 
#' @return Logical vector with a length of \code{1}. 
#' 
#' @export
any_duplicated <- function(x) {
  if_else(anyDuplicated(x) == 0L, FALSE, TRUE)
}
