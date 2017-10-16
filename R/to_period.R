#' Function to calculate a \strong{lubridate} \code{period} date class based on
#' two dates. 
#' 
#' @param date_one First date. 
#' 
#' @param date_two Second date. 
#' 
#' @param round Number of decimal points to round to. Default is no rounding. 
#' 
#' @param as.character Should the period be formated and returned as a character?
#' 
#' @return \code{Period} or character vector. 
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \code{\link{as.period}}
#' 
#' @export
to_period <- function(date_one, date_two, round = FALSE, as.character = FALSE) {
  
  # Interval first
  x <- lubridate::as.interval(date_one, date_two)
  
  # Then period
  x <- lubridate::as.period(x)
  
  # Formating tasks
  if (!is.na(round)) x <- round(x, round)
  if (as.character) x <- format(x)
  
  return(x)
  
}
