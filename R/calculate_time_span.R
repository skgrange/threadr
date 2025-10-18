#' Function to calculate time span between dates. 
#' 
#' @author Stuart K. Grange. 
#' 
#' @param date_one First date. 
#' 
#' @param date_two Second date. 
#' 
#' @param format Type of return. 
#' 
#' @param round Number of digits to round return to.  
#' 
#' @param as.character Should the return be a character vector. 
#' 
#' @return Vector length of 1. Can be of type numeric, hms, duration, period, 
#' or character. 
#' 
#' @examples 
#' 
#' # Define some dates
#' date_one <- lubridate::ymd_h("2019-02-03 5", tz = "UTC")
#' date_two <- lubridate::ymd_hms("2019-04-12 6:54:55.98565", tz = "UTC")
#' 
#' # Find spans in different formats
#' # Seconds as default
#' calculate_time_span(date_one, date_two)
#' 
#' # Other formats
#' calculate_time_span(date_one, date_two, format = "hms")
#' calculate_time_span(date_one, date_two, format = "duration")
#' calculate_time_span(date_one, date_two, format = "period")
#' calculate_time_span(date_one, date_two, format = "days")
#' 
#' # Use round to reduce precision
#' calculate_time_span(date_one, date_two, format = "period", round = 0)
#' 
#' @export
calculate_time_span <- function(date_one, date_two, 
                                format = c(
                                  "seconds", "hms", "duration", "period", "days"
                                ), 
                                round = NA,
                                as.character = FALSE) {
  
  # Push dates to POSIXct
  if (lubridate::is.Date(date_one)) {
    date_one <- as.POSIXct(date_one, tz = "UTC")
  }
  
  if (lubridate::is.Date(date_two)) {
    date_two <- as.POSIXct(date_two, tz = "UTC")
  }
  
  # Check that inputs are POSIXct
  stopifnot(lubridate::is.POSIXt(date_one), lubridate::is.POSIXt(date_two))
  format <- stringr::str_to_lower(format[1])
  
  # To seconds, generally what is used for calculations
  if (format != "period") x <- as.numeric(date_two) - as.numeric(date_one)
  
  if (format == "hms") {
    x <- hms::as_hms(x)
  } else if (format == "duration") {
    x <- lubridate::as.duration(x)
  } else if (format == "period") {
    x <- lubridate::as.interval(date_one, date_two)
    x <- lubridate::as.period(x)
  } else if (format == "days") {
    x <- x / threadr::seconds_in_a_day()
  }
  
  # Round return
  if (!is.na(round)) {
    x <- round(x, digits = round)
  }
  
  # To character
  if (as.character) {
    x <- format(x)
  }
  
  return(x)
  
}
