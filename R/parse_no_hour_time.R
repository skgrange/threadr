#' Function to parse a time with greater than 60 minuntes to a period. 
#' 
#' \code{parse_no_hour_time} will format and parse a time such as \code{124.17}
#' (124 minutes and 17 seconds) to a period with hour, minutes, and seconds. 
#' 
#' @param x Vector of times in no hour format.  
#' 
#' @param format What format of return is desired? The options are 
#' \code{"period"}, \code{"hms"}, or \code{"seconds"}. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Period, hms, or numeric vector.
#' 
#' @seealso \code{\link{period_to_date}}, \code{\link{period_to_string}} 
#' 
#' @examples 
#' 
#' # Parse a times with no hour
#' parse_no_hour_time(c(124.17, "89:22"))
#' 
#' # As hms
#' parse_no_hour_time(c(124.17, "89:22"), format = "hms")
#' 
#' # Or as seconds
#' parse_no_hour_time(c(124.17, "89:22"), format = "seconds")
#' 
#' 
#' @export
parse_no_hour_time <- function(x, format = "period") {
  
  # Check input
  stopifnot(format %in% c("period", "hms", "seconds"))
  
  # Clean input
  x <- stringr::str_replace_all(x, "\\.", ":")
  x <- if_else(stringr::str_count(x, ":") == 1, stringr::str_c("00:", x), x)
  
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
  x <- lubridate::hms(time_string)

  # To other data types,
  if (format == "hms") {
    x <- hms::as_hms(as.numeric(x))
  } else if (format == "seconds") {
    x <- as.numeric(x)
  }
  
  return(x)
  
}
