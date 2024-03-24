#' Functions to determine and group streaks/consecutive events.
#' 
#' Streaks are also known as run lengths. 
#' 
#' @author Stuart K. Grange
#' 
#' @param event A logical vector indicating if an event occurred or not. 
#' 
#' @param only_true Should only events that are \code{TRUE} be returned as 
#' streaks? 
#' 
#' @param streak_max In the case of \code{streak_group}, the output from the 
#' \code{streak_max} function. 
#' 
#' @return Integer vector with the length of \code{event} or \code{streak_max}.
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
    x <- if_else(!event, 0L, x)
  }
  
  return(x)
  
}


#' @rdname streak
#' @export
streak_max <- function(event) {
  
  # Check input
  stopifnot(is.logical(event))
  
  # Determine the locations where the vector changed
  x <- rle(event)
  
  # Create a vector where the streak length is replicated
  x <- rep(x$lengths, times = x$lengths)
  
  return(x)
  
}


#' @rdname streak
#' @export
streak_group <- function(streak_max) {
  
  # Determine deltas between streak max values 
  streak_max_change <- lag_delta(streak_max, na_as_zero = TRUE)
  
  # Has the delta changed? 
  streak_max_change <- streak_max_change != 0L
  
  # Calculate a grouping integer
  streak_group <- cumsum(streak_max_change) + 1L
  
  return(streak_group)
}


#' Function to add streak variables to a tibble. 
#' 
#' @author Stuart K. Grange
#' 
#' @param df Tibble or data frame containing a logical \code{event} variable. 
#' 
#' @return Tibble, \code{df} with additional streak variables. 
#' 
#' @seealso \code{\link{streak}}, \code{\link{streak_max}}, 
#' \code{\link{streak_group}}
#' 
#' @export
add_steak_variables <- function(df) {
  
  # Check if the event variable exists
  stopifnot("event" %in% names(df))
  
  # Calculate the streak variables
  df %>% 
    mutate(streak = streak(event),
           streak_max = streak_max(event),
           streak_group = streak_group(streak_max))
  
}
