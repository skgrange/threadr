#' Function to transform a irregular time-series to a continuous time-series. 
#' 
#' \code{time_pad_irregular} is usually used when a irregularly sampled 
#' variables are to be analysed alongside variables which are monitored at 
#' hourly or daily intervals. 
#'
#' \code{time_pad_irregular} will pad a variable's time-series and then 
#' push values forwards to replace missing data after padding. This is known as 
#' a "last observation carried forward" (locf) process. 
#' 
#' It is not recommended that the transformed variables are used for 
#' aggregations. \code{time_pad_irregular} will introduce values into a 
#' time-series and if these are aggregated, in most cases, this will be a flawed
#' procedure.
#' 
#' \code{time_pad_irregular} requires a data frame with a \code{"date"} and
#' \code{"date_end"} variables because these are used for the locf process. 
#' 
#' @param df Data frame containing an irregular time-series with \code{"date"} 
#' and \code{"date_end"} variables. 
#' 
#' @param interval Interval to pad the time-series to? This will almost always
#' be \code{"hour"} or \code{"day"}.
#' 
#' @param by Should \code{time_pad_irregular} apply the padding function to 
#' groups within \code{df}? This is helpful when there are many sites/other 
#' identifiers within \code{df} which need to be padded individually. 
#' 
#' @seealso \code{\link{na.locf}}, \code{\link{round_date_interval}}, 
#' \code{\link{time_pad}}, \code{\link{timeAverage}}, \code{\link{round_date}}
#' 
#' @import dplyr
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' # Make a continuous time-series
#' data_benzene_continuous <- time_pad_irregular(data_benzene, "hour")
#' }
#' 
#' @export
time_pad_irregular <- function (df, interval, by = NA) {
  
  # Check variables
  if (!all(c("date", "date_end") %in% names(df))) {
    stop("Data frame must include `date` and `date_end` variables.", 
         call. = FALSE)
  }
  
  # Check interval
  if (!interval %in% c("hour", "day")) {
    stop("Interval must be `hour` or `day`.", call. = FALSE)
  }
  
  if (is.na(by[1])) {
    # No group-by needed
    df <- irregular_padder(df, interval, by)
    
  } else {
    # Apply function to all site combinations
    # Get around non-standard evaluation
    list_dots <- lapply(by, as.symbol)
    
    # Pad by group(s)
    df <- df %>% 
      group_by_(.dots = list_dots) %>%
      do(irregular_padder(., 
                          interval = interval, 
                          by = by))

  }
  
  # Return
  df
  
}


# The worker
# 
# No export
irregular_padder <- function (df, interval, by = NA) {
  
  # Get identifiers
  if (!is.na(by[1])) {
    # Get identifiers
    data_by <- get_identifiers(df, by = by)
  }
  
  # Get indices
  # Numeric variables
  index_numeric <- sapply(df, is.numeric)
  # To index
  index_numeric <- which(index_numeric)
  
  # Date index
  index_date <- which(names(df) %in% c("date", "date_end"))
  
  # Select what is needed
  df <- df[ , c(index_date, index_numeric)]
  
  # Add observation number, used for arranging
  df <- add_row_numbers(df)
  
  # Make data longer
  df <- tidyr::gather_(df, key_col = "date_type", value_col = "date",
                       gather_col = c("date", "date_end"))
  
  # Arrange by observation number
  df <- dplyr::arrange(df, row_number)
  
  # Round dates down, time-ending assumption
  df$date <- lubridate::floor_date(df$date, interval)
  
  # Create a leading variable
  df$date_ahead <- dplyr::lead(df$date)
  # Catch final NA
  df$date_ahead[nrow(df)] <- df$date_ahead[nrow(df) - 1]
  
  # If start and end dates are identical, push end date back a unit so there are
  # no duplicate dates in the time series
  if (interval == "day") {
    df$date <- ifelse(df$date_type == "date_end" & 
      df$date == df$date_ahead, df$date - lubridate::days(1), df$date)
    
    # Catch the conversion, to-do check if TZ is robust enough
    df$date <- as.POSIXct(df$date, origin = "1970-01-01", tz = "UTC")
    
  }
  
  if (interval == "hour") {
    df$date <- ifelse(df$date_type == "date_end" & 
      df$date == df$date_ahead, df$date - lubridate::hours(1), df$date)
    
    # Catch the conversion
    df$date <- as.POSIXct(df$date, origin = "1970-01-01", tz = "UTC")
    
  }
  
  # Pad timeseries 
  df <- time_pad(df, interval)
  
  # Drop temporary variables
  df <- dplyr::select(df, -row_number, -date_type, -date_ahead)
  
  # Push observations forward
  df[, -1] <- lapply(df[, -1], locf)
  
  # Add identifiers after padding
  if (!is.na(by[1])) {
    df <- cbind(df, data_by)
  }
  
  # Return
  df
  
}

