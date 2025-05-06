#' Function to summarise a date vector. 
#' 
#' @param date A \code{POSIXct} or \code{Date} vector. 
#' 
#' @author Stuart K. Grange.
#' 
#' @return A tibble with one row. 
#' 
#' @seealso \code{\link{calculate_ci}}, \code{\link{calculate_quantiles}},
#' \code{\link{calculate_range}}
#' 
#' @export
calculate_date_range <- function(date) {
  
  # Check input
  stopifnot(inherits(date, c("Date", "POSIXct")))
  
  # Get lengths of vector
  n_all <- length(date)
  n <- length(date[!is.na(date)])
  
  # Calculate start and end
  min <- min(date, na.rm = TRUE)
  max <- max(date, na.rm = TRUE)
  
  # Calculate time span
  range <- calculate_time_span(min, max, format = "seconds")
  range_period <- calculate_time_span(min, max, format = "period")
  
  # Bind together into a tibble
  df <- tibble(
    n_all,
    n, 
    min,
    max,
    range,
    range_period
  )
  
  return(df)
  
}
