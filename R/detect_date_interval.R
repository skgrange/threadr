#' Function to determine averaging period in a date vector.
#' 
#' @param date A date vector. 
#' @param skip Number of elements in \code{date} to skip before detecting 
#' interval. 
#' @param n Number of elements in \code{date} to evaluate to detect the interval. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
detect_date_interval <- function(date, skip = 1, n = 100) {
  
  if (!lubridate::is.POSIXt(date))
    stop("'date' must be a POSIXt date.", call. = FALSE)
  
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
  
  # Unexported function
  seconds <- mode(seconds)
  
  # Return
  seconds
  
  # # Default
  # period <- "other"
  # 
  # # Known periods
  # if (all(seconds == 1)) period <- "second"
  # if (all(seconds == 60)) period <- "minute"
  # if (all(seconds == 600)) period <- "ten_minute"
  # if (all(seconds == 900)) period <- "fifteen_minute"
  # if (all(seconds == 3600)) period <- "hour"
  # if (all(seconds == 86400)) period <- "day"
  # if (all(seconds %in% c(2419200, 2678400, 2592000, 2505600))) period <- "month"
  # 
  # # Return
  # period
  
}
