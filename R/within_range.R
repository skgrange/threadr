#' Function to test if a vector is within one, or many ranges. 
#' 
#' \code{within_range} works well for testing Unix time too.  
#' 
#' @param x Numeric vector to test. 
#' 
#' @param start Start of range. 
#' 
#' @param end End of range.
#' 
#' @return Logical vector of length of \code{x}. 
#' 
#' @seealso \code{\link{which}}
#' 
#' @examples
#' 
#' # Test a single range
#' within_range(1:10, 8, 9)
#' 
#' # Test many ranges
#' within_range(1:100, c(1, 5), c(2, 9))
#'  
#' @author Stuart K. Grange
#' 
#' @export
within_range <- function(x, start, end) {
  
  if (!length(start) == length(end)) {
    stop("`start` and `end` must be the same length.", call. = FALSE)
  }
  
  # Do the test
  x <- purrr::map2(start, end, ~within_range_worker(x, .x, .y)) %>% 
    do.call(rbind, .) %>% 
    apply(2, function(z) any(z))
  
  return(x)
  
}


within_range_worker <- function(x, start, end) {
  if_else(x >= start & x <= end, TRUE, FALSE)
}
