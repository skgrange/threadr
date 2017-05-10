#' Function to transform a irregular time-series to a continuous time-series. 
#' 
#' \code{time_pad_irregular} is usually used when irregularly sampled variables 
#' are to be analysed alongside variables which are monitored at regular hourly 
#' or daily intervals. 
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
#' be \code{"hour"} or \code{"day"} but also accepts \code{"sec"} and 
#' \code{"min"}.
#' 
#' @param by Should \code{time_pad_irregular} apply the padding function to 
#' groups within \code{df}? This is helpful when there are many sites/other 
#' identifiers within \code{df} which need to be padded individually. 
#' 
#' @param na.rm Should \code{NA} values be removed before the locf process? If 
#' \code{TRUE}, this will result in a time-series with no \code{NA} values but 
#' may insert observations where they were originally represented as missing. 
#' 
#' @seealso \code{\link{na.locf}}, \code{\link{round_date_interval}}, 
#' \code{\link{time_pad}}, \code{timeAverage}, \code{\link{round_date}}
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' # Make a continuous time-series
#' data_benzene_continuous <- time_pad_irregular(data_benzene, "day")
#' }
#' 
#' @export
time_pad_irregular <- function (df, interval, by = NA, na.rm = FALSE) {
  
  # Replace dot if used
  names(df) <- stringr::str_replace(names(df), "\\bdate.end\\b", "date_end")
  
  # Check if variables exist
  if (!all(c("date", "date_end") %in% names(df)))
    stop("Data frame must include `date` and `date_end` variables.", 
         call. = FALSE)
  
  # Check interval
  if (!interval %in% c("sec", "min", "hour", "day"))
    stop("Interval must be `sec`, `min`, `hour`, or `day`.", call. = FALSE)
  
  if (is.na(by[1])) {
    
    # No group-by needed
    df <- irregular_padder(df, interval, by, na.rm)
    
  } else {
    
    # Apply function to all site combinations
    # Get around non-standard evaluation
    list_dots <- lapply(by, as.symbol)
    
    # Pad by group(s)
    df <- df %>% 
      dplyr::group_by_(.dots = list_dots) %>%
      dplyr::do(irregular_padder(., 
                                 interval = interval, 
                                 by = by,
                                 na.rm = na.rm))

  }
  
  # Return
  df
  
}


# The worker
# 
# No export
irregular_padder <- function (df, interval, by = NA, na.rm) {
  
  # Get identifiers
  if (!is.na(by[1])) data_by <- get_identifiers(df, by = by)

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
    
  }
  
  if (interval == "hour") {
    
    df$date <- ifelse(df$date_type == "date_end" & 
      df$date == df$date_ahead, df$date - lubridate::hours(1), df$date)

  }
  
  if (interval == "min") {
    df$date <- ifelse(df$date_type == "date_end" & 
      df$date == df$date_ahead, df$date - lubridate::minutes(1), df$date)
    
  }
  
  if (interval == "sec") {
    
    df$date <- ifelse(df$date_type == "date_end" & 
      df$date == df$date_ahead, df$date - lubridate::seconds(1), df$date)
    
  }
  
  # Catch the conversion, to-do check if TZ is robust enough and if this works
  # when the manipulation of date_end is not needed.
  df$date <- as.POSIXct(df$date, origin = "1970-01-01", tz = "UTC")
  
  # Store NAs as strings so they are not pushed forwards
  if (!na.rm) {
    
    # Get numeric index
    index_numeric <- sapply(df, is.numeric)
    index_numeric["row_number"] <- FALSE
	
	# Replace/case conversion
    df[index_numeric] <- lapply(df[index_numeric], stringr::str_replace_na)
	
  }
  
  # Pad time-series 
  df <- time_pad(df, interval)
  
  # Drop temporary variables
  df <- dplyr::select(df, -row_number, -date_type, -date_ahead)
  
  # Push observations forwards, ifs avoid warnings
  if (ncol(df) == 2) {
    
    df[, 2] <- locf(df[, 2])
    
  } else {
    
    df[, -1] <- lapply(df[, -1], locf)
    
  }
  
  # Case conversion if numeric variables were pushed to characters
  if (!na.rm) {
    
    if (ncol(df) == 2) {
      
      df[, 2] <- type.convert(df[, 2], as.is = TRUE)
      
    } else {
      
      df[, -1] <- lapply(df[, -1], function (x) type.convert(x, as.is = TRUE))
      
    }
    
  }
  
  # Add identifiers after padding
  if (!is.na(by[1])) df <- cbind(df, data_by)
  
  # Return
  df
  
}
