#' Function to determine streaks/consecutive events. 
#' 
#' @author Stuart K. Grange
#' 
#' @param event A logical vector indicating if an event occurred or not. 
#' 
#' @param only_true Should only events that are \code{TRUE} be returned as 
#' streaks? 
#' 
#' @return Numeric vector with the length of \code{event}.
#' 
#' @seealso \code{\link{rle}}
#' 
#' @export
streak <- function(event, only_true = TRUE) {
  
  # Check input
  stopifnot(is.logical(event))
  
  # Determine the locations where the vector changed
  x <- rle(event)
  
  # Build sequence
  x <- sequence(x$lengths)
  
  # Reset sequence if the event did not occur
  if (only_true) {
    x <- if_else(!event, 0, x)
  }
  
  return(x)
  
}
