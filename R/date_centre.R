#' Function to calculate the centre point of two dates.
#' 
#' @author Stuart K. Grange. 
#' 
#' @param date_start Start date.
#' 
#' @param date_end End date. 
#' 
#' @return \code{POSIXct} vector with a length of one. 
#' 
#' @examples 
#' 
#' # Define some dates
#' date_one <- lubridate::ymd_h("2019-02-03 5", tz = "UTC")
#' date_two <- lubridate::ymd_hms("2019-04-12 6:54:55.98565", tz = "UTC")
#' 
#' # Calculate centre point
#' date_centre(date_one, date_two)
#' 
#' @export
date_centre <- function(date_start, date_end) {
  x <- (as.numeric(date_start) + as.numeric(date_end)) / 2
  x <- parse_unix_time(x, tz = "UTC")
  return(x)
}