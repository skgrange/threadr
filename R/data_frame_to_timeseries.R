#' Function to promote a data frame to a time-series object. 
#' 
#' @author Stuart K. Grange
#' 
#' @param df Data frame containing time-series data with a variable named 
#' \code{"date"}. 
#' 
#' @param tz Time-zone of created time-series object. Default is \code{"UTC"}. 
#' 
#' @return Named list of time-series objects. 
#' 
#' @export
data_frame_to_timeseries <- function(df, tz = "UTC") {
  
  # Check if input has a date variable
  if (!"date" %in% names(df))
    stop("Data frame must contain a date variable/column and must be named 'date'.",
         call. = FALSE)
  
  # Get date
  date <- df$date
  
  # Select only numeric variables
  index <- sapply(df, is.numeric)
  df <- df[index]
  
  # Promote to time-series, will create a named list
  list_ts <- lapply(df, function(x) ts_promoter(date, x, tz))
  
  # Return
  list_ts
  
}


# Function to promote variables
ts_promoter <- function(date, variable, tz)
  xts::xts(variable, order.by = date, tz = tz)