#' Function to pad time-series.
#'
#' \code{time_pad} use is similar to \strong{openair}'s \code{timeAverage}, but
#' the aggregation of values does not occur. \code{time_pad} does not drop 
#' non-numerical variables, can include identifiers post-pad, and can start and
#' end a padded time-series at a "nice place", for example, at the beginning of
#' an hour or day. 
#' 
#' \code{time_pad} pads a time-series by calculating the maximum and minimum 
#' dates within a time-series and then generating a uniform date sequence 
#' between the maximum and minimum dates. This date sequence is then joined to 
#' the input data frame and the missing values are represented as \code{NA}. 
#' 
#' @param df A data frame including parsed dates. The date variable/column must
#' be named \code{date}.
#' 
#' @param interval Interval of returned time series. Some examples could be: 
#' \code{"min"} \code{"hour"}, \code{"day"}, \code{"month"}, \code{"year"} but 
#' multiples such as \code{"5 min"} work too. \code{interval} can also be a 
#' numeric value such as \code{0.5} which is useful for sub-second padding. 
#' 
#' @param by Should \code{time_pad} apply the padding function to groups within
#' \code{df}? This is helpful when there are many sites/other identifiers within
#' \code{df} which need to be padded individually. 
#' 
#' @param round What date-unit should the first and last observations be rounded
#' to? This allows the padded time-series to begin and end at a "nice place". 
#' Examples are \code{"hour"}, \code{"day"}, \code{"month"}, and \code{"year"}.
#' 
#' @param full Should the date joining use the \code{full_join} function? If 
#' \code{TRUE}, no input dates will be lost but the default is \code{FALSE}. 
#' 
#' @param warn Should the function give a warning when dates are duplicated? 
#' Default is \code{TRUE}. 
#' 
#' @seealso See \code{\link{round_date_interval}}, \code{timeAverage}, 
#' \code{\link{round_date}}, \code{\link{left_join}}
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' # Pad time-series so every minute is present
#' data_nelson_pad <- time_pad(data_nelson, interval = "min", round = "day")
#' 
#' # Keep identifying variables "site" and "sensor"
#' data_ozone_sensor_pad <- time_pad(
#'   data_ozone_sensor, 
#'   interval = "hour", 
#'   by = c("site", "sensor")
#' )
#' 
#' }
#' 
#' @export
time_pad <- function(df, interval = "hour", by = NA, round = NA, 
                     full = FALSE, warn = TRUE) {
  
  # Check input
  if (nrow(df) == 0) {
    stop("Input data frame has no observations...", call. = FALSE)
  }
  
  # Check if input has a date variable
  if (!"date" %in% names(df)) {
    stop(
      "Input must contain a date variable/column and must be named `date`...",
      call. = FALSE
    )
  }
  
  # NA check
  if (any(is.na(df$date))) {
    stop("`date` must not contain missing (`NA`) values...", call. = FALSE)
  }
  
  # Check class
  if (!lubridate::is.POSIXct(df$date)) {
    stop("`date` must be a POSIXct date...", call. = FALSE)
  }
  
  # Get variable order
  variables <- names(df)
  
  if (is.na(by[1])) {
    
    # No group-by needed
    df <- time_pad_worker(
      df, 
      interval = interval, 
      by = by, 
      round = round, 
      full = full, 
      warn = warn
    ) %>% 
      select(!!variables, 
             everything())
    
  } else {
    
    # Pad by group
    df <- df %>% 
      dplyr::group_by_at(by) %>%
      do(
        time_pad_worker(
          ., 
          interval = interval, 
          by = by,
          round = round, 
          full = full,
          warn = warn
        )
      ) %>% 
      select(!!variables, 
             everything()) %>% 
      ungroup()
    
  }
  
  return(df)
  
}


# The worker
# 
# No export
time_pad_worker <- function(df, interval, by, round, merge, full, warn) {
  
  # Switch interval
  interval <- stringr::str_trim(interval)
  interval <- if_else(interval %in% c("minute", "minutes"), "min", interval)
  interval <- if_else(interval %in% c("second", "seconds"), "sec", interval)
  interval <- if_else(interval == "hours", "hour", interval)
  
  # Attempt to make interval a numeric value for sub-second padding
  interval <- tryCatch({
    as.numeric(interval)
  }, warning = function(w) {
    interval
  })
  
  # Store identifiers in a single row data frame
  if (!is.na(by[1])) {
    
    # Get identifiers
    df_by <- df %>% 
      dplyr::slice(1) %>% 
      dplyr::select_at(by)
    
    # Drop identifiers from input
    df <- dplyr::select_at(df, dplyr::vars(-by))
    
  }
  
  # Find the start and end of the date sequence
  if (is.na(round)) {
    # No date rounding, use date values in df
    date_start <- min(df$date)
    date_end <- max(df$date)
  } else {
    # Date rounding
    date_start <- lubridate::floor_date(min(df$date), round)
    date_end <- lubridate::ceiling_date(max(df$date), round)
  }
  
  # Create the sequence of dates
  date_sequence <- seq(date_start, date_end, by = interval)
  
  # Remove final observation if ceiling rounded
  if (!is.na(round)) date_sequence <- date_sequence[-length(date_sequence)]
  
  # To data frame
  date_sequence <- tibble(date = date_sequence)
  
  # Do the padding
  if (full) {
    df <- dplyr::full_join(date_sequence, df, by = "date")
    df <- arrange(df, date)
  } else {
    df <- left_join(date_sequence, df, by = "date")
  }
  
  # Bind identifiers again
  if (!is.na(by[1])) {
    df <- df_by %>% 
      dplyr::slice(rep(1:dplyr::n(), each = nrow(df))) %>% 
      dplyr::bind_cols(df)
  }
  
  # Ensure return is a tibble
  df <- as_tibble(df)
  
  # Message
  if (warn) {
    if (any(duplicated(df$date))) {
      warning("Duplicated dates detected...", call. = FALSE)
    }
  }
  
  return(df)
  
}
