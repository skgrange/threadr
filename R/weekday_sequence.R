#' Function to generate a sequence of weekdays. 
#' 
#' @param n Number of days for the sequence to return. 
#' 
#' @param monday Should the sequence start on a Monday? Default is \code{TRUE} 
#' and set to \code{FALSE} if the sequence should start on Sunday. 
#' 
#' @param factor Should the return be an ordered factor? Default is \code{TRUE}.
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' 
#' # Generate weekday sequence as ordered factors
#' weekday_sequence()
#' 
#' # Ordered factors starting on Sunday
#' weekday_sequence(monday = FALSE)
#' 
#' # As characters
#' weekday_sequence(factor = FALSE)
#'  
#' }
#' 
#' @export
weekday_sequence <- function (n = 7, monday = TRUE, factor = TRUE) {
  
  # Create sequence
  days <- factor(weekdays(as.Date(seq(n), origin = "1950-01-01")))
  
  # Create ordered factors
  if (factor) {
    
    # Order
    if (monday) {
      
      days <- ordered(days, levels = c("Monday", "Tuesday", "Wednesday", 
        "Thursday", "Friday", "Saturday", "Sunday"))
      
    } else {
      
      days <- ordered(days, levels = c("Sunday", "Monday", "Tuesday", 
        "Wednesday", "Thursday", "Friday", "Saturday"))
      
    }
    
  } else {
    
    # Make character vector
    days <- as.character(days)
    
  }
  
  # Return
  days
  
}
