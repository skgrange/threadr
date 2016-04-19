#' Function to apply rolling mean to a vector. 
#' 
#' @param x Numeric vector. The order of \code{x} should be checked to ensure 
#' behaviour is as expected. 
#' 
#' @param width Width of margin to apply function. 
#' 
#' @param align Should the index of the result be centered, left- or 
#' right-aligned? Default is \code{"center"}. 
#' 
#' @param na.rm Should \code{NA}s be removed for the aggregation. Default is 
#' \code{TRUE}, but setting to \code{FALSE} will be faster. 
#' 
#' @seealso \code{\link{mean}}, \code{\link{rollapply}}, \code{\link{rollmean}}
#' 
#' @author Stuart K. Grange
#' 
#' @export
simple_moving_average <- function(x, width, align = "center", na.rm = TRUE) {
  
  if (na.rm) {
    
    # Omit NAs
    x <- zoo::rollapply(x, width, align = align, fill = NA,
                        function(y) mean(y, na.rm = TRUE))
    
  } else { 
    
    # Optimised version, much faster
    x <- zoo::rollmean(x, width, align = align, fill = NA)
    
  }
  
  # Return
  x
  
}
