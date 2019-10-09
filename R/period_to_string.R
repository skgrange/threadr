#' Function to format \strong{lubridate} periods into a \code{HH:MM:SS} format. 
#' 
#' @author Stuart K. Grange
#' 
#' @param period A vector of \strong{lubridate}'s periods. 
#' 
#' @export
period_to_string <- function(period) {
  
  period <- period_to_date(period)
  
  # Format for printing
  period <- format(period, format = "%H:%M:%OS")
  
  return(period)
  
}


#' Function convert a lubridate period to a POSIXct date.
#' 
#' @author Stuart K. Grange
#' 
#' @param period A vector of \strong{lubridate}'s periods. 
#' 
#' @export
period_to_date <- function(period) {
  
  # Only stings bitte
  period <- as.character(period)
  
  # Pad if needed
  period <- ifelse(
    !stringr::str_detect(period, "H"), 
    stringr::str_c("0H ", period), 
    period
  )
  
  # Parse again, as POSIXct
  period <- lubridate::parse_date_time(
    period, 
    c("HMOS", "MOS", "HMS", "MS"), 
    quiet = FALSE
  )
  
  return(period)
  
}
