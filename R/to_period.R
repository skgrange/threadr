#' Function to calculate a \strong{lubridate} \code{period} date class based on
#' two dates. 
#' 
#' @param date_one First date. 
#' 
#' @param date_two Second date. 
#' 
#' @param round Number of decimal points to round to. Default is no rounding. 
#' 
#' @return \code{Period} vector. 
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \code{\link{as.period}}
#' 
#' @export
to_period <- function(date_one, date_two, round = FALSE) {
  
  x <- date_one %>% 
    lubridate::as.interval(date_two) %>% 
    lubridate::as.period()
  
  if (!is.na(round)) x <- round(x, round)
  
  return(x)
  
}
