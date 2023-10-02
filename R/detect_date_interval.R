#' Function to determine period of a date vector.
#' 
#' @param date A date vector of \code{POSIXt} class.  
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
#' @return Integer or character vector with a length of \code{1}.
#' 
#' @export
detect_date_interval <- function(date, skip = 1L, n = 100L, text_return = FALSE) {
  
  # Check
  if (!lubridate::is.POSIXt(date)) {
    cli::cli_abort("`date` must be a POSIXt date.")
  }
  
  # A catch for vectors with fewer elements than skip
  if (length(date) <= skip) skip <- 0L
  
  # Get vectors
  # Skip if needed
  date <- date[skip:length(date)]
  
  # Filter
  date <- head(date, n)
  date_lag <- dplyr::lag(date)
  
  # Calculate difference
  seconds <- difftime(date, date_lag, units = "secs")
  seconds <- as.integer(seconds)
  seconds <- seconds[!is.na(seconds)]
  
  # Most common occurrence
  seconds <- mode_average(seconds, na.rm = TRUE)
  
  if (!text_return) {
    return(seconds)
  } else {
    
    # Default
    period <- "unknown"
    
    # Missing-ness test, when length is one
    if (length(seconds) == 1L && is.na(seconds)) {
      return(period)
    }
    
    # Known periods
    period <- dplyr::case_when(
      all(seconds == 1L) ~ "second",
      all(seconds == 60L) ~ "minute",
      all(seconds == 300L) ~ "five_minute",
      all(seconds == 600L) ~ "ten_minute",
      all(seconds == 900L) ~ "fifteen_minute",
      all(seconds == 1800L) ~ "half_hour",
      all(seconds == 3600L) ~ "hour",
      all(seconds == 86400L) ~ "day",
      all(seconds %in% c(2419200L, 2678400L, 2592000L, 2505600L)) ~ "month",
      .default = "unknown"
    )
    
    # Return text
    return(period)
    
  }
  
}
