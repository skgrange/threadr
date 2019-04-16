#' Function to find closest index in a vector. 
#' 
#' @param vector Vector to find closest value within.  
#' 
#' @param value Value to find closest indices for. Must be a vector with length 
#' of 1.  
#' 
#' @param na.rm Should \code{NA}s be ommited? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Integer vector. 
#' 
#' @section \code{\link{which}}, \code{\link{which.min}}
#' 
#' @examples 
#' 
#' # A vector with no 17
#' x <- c(
#'   15, 15.5, 16, 16, 16, 18, 18, 18, 18, 18, 18.5, 19, 19.5, 20, 20, 20, 20.5, 
#'   21, 21, 21, 21, 21
#' )
#' 
#' # Using base which returns an empty integer
#' which(x == 17)
#' 
#' # All values which are closest
#' which_closest(x, 17)
#' 
#' # Just a single value which is closest
#' which_closest(x, 17)[1]
#' 
#' # Get element, use as a filter
#' x[which_closest(x, 17)[1]]
#' 
#' @export
which_closest <- function(vector, value, na.rm = FALSE) {
  stopifnot(length(value) == 1)
  which(abs(vector - value) == min(abs(vector - value), na.rm = na.rm))
}
