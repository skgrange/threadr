#' Function to determine averaging period in a date vector.
#' 
#' @param date A date vector of POSIXt class.  
#' 
#' @param skip Number of elements in \code{date} to skip before detecting 
#' interval. 
#' 
#' @param n Number of elements in \code{date} to evaluate to detect the interval. 
#' 
#' @param text_return Should the date interval be evaluated and a friendly text-
#' based return be attempted? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Numeric or text vector with a length of 1. 
#' 
#' @export
detect_date_interval <- function(date, skip = 1, n = 100, text_return = FALSE) {
  
  if (!lubridate::is.POSIXt(date)) {
    stop("'date' must be a POSIXt date.", call. = FALSE)
  }
  
  # Get vectors
  # Skip if needed
  date <- date[skip:length(date)]
  
  # Filter
  date <- head(date, n)
  date_lag <- dplyr::lag(date)
  
  # Calculate difference
  seconds <- difftime(date, date_lag, units = "secs")
  seconds <- as.numeric(seconds)
  seconds <- seconds[!is.na(seconds)]
  
  # Most common occurance
  seconds <- mode_average(seconds, na.rm = TRUE)
  
  if (!text_return) {
    
    # Return now
    return(seconds)
    
  } else {
    
    # Default
    period <- "unknown"
    
    # Known periods
    if (all(seconds == 1)) {
      period <- "second"
    } else if (all(seconds == 60)) {
      period <- "minute"
    } else if (all(seconds == 300)) {
      period <- "five_minute"
    } else if (all(seconds == 600)) {
      period <- "ten_minute"
    } else if (all(seconds == 900)) {
      period <- "fifteen_minute"
    } else if (all(seconds == 1800)) {
      period <- "half_hour"
    } else if (all(seconds == 3600)) {
      period <- "hour"
    } else if (all(seconds == 86400)) {
      period <- "day"
    } else if (all(seconds %in% c(2419200, 2678400, 2592000, 2505600))) {
      period <- "month"
    }
    
    # Return text
    return(period)
    
  }
  
}
