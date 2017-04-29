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
  
  # Return
  period
  
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
  period <- ifelse(!stringr::str_detect(period, "H"), 
                   stringr::str_c("0H ", period), period)
  
  # Parse again, as POSIXct
  period <- lubridate::parse_date_time(period, c("HMOS", "MOS", "HMS", "MS"), 
                                       quiet = FALSE)
  
  # Return
  period
  
}


#' Function to parse a time with greater than 60 minuntes to a period. 
#' 
#' \code{parse_no_hour_time} will format and parse a time such as \code{124.17}
#' (124 minutes and 17 seconds) to a period with hour, minutes, and seconds. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Vector of periods; a \strong{lubridate} data type.  
#' 
#' @seealso \code{\link{period_to_date}}, \code{\link{period_to_string}} 
#' 
#' @examples 
#' \dontrun{
#' 
#' # Parse a times with no hour
#' parse_no_hour_time(c(124.17, "89:22"))
#' 
#' }
#' 
#' @export
parse_no_hour_time <- function(x) {
  
  # Clean input
  x <- stringr::str_replace_all(x, "\\.", ":")
  x <- ifelse(stringr::str_count(x, ":") == 1, stringr::str_c("00:", x), x)
  
  # Get date pieces
  time_split <- stringr::str_split_fixed(x, ":", 3)
  seconds <- as.numeric(time_split[, 3])
  minutes <- as.numeric(time_split[, 2])
  hours_decimal <- minutes / 60
  hours_whole <- floor(hours_decimal)
  hours_decimal_fraction <- stringr::str_split_fixed(hours_decimal, "\\.", 2)[, 2]
  hours_decimal_fraction <- stringr::str_c("0.", hours_decimal_fraction)
  hours_decimal_fraction <- as.numeric(hours_decimal_fraction)
  minutes <- hours_decimal_fraction * 60
  minutes <- round(minutes)
  
  # Build the string
  time_string <- stringr::str_c(hours_whole, minutes, seconds, sep = ":")
  
  # Parse
  period <- lubridate::hms(time_string)
  
  return(period)
  
}
