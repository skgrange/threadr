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
#' multiples such as \code{"5 min"} work too. 
#' 
#' @param by Should \code{time_pad} apply the padding function to groups within
#' \code{df}? This is helpful when there are many sites/other identifiers within
#' \code{df} which need to be padded individually. 
#' 
#' @param round What date-unit should the first and last observations be rounded
#' to? This allows the padded time-series to begin and end at a "nice place". 
#' Examples are \code{"hour"}, \code{"day"}, \code{"month"}, and \code{"year"}.
#' 
#' @param warn Should the function give a warning when dates are duplicated? 
#' Default is \code{TRUE}. 
#' 
#' @seealso See \code{\link{round_date_interval}}, \code{\link{timeAverage}}, 
#' \code{\link{round_date}}, \code{\link{left_join}}
#' 
#' @author Stuart K. Grange
#' 
#' @import dplyr
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' # Pad time-series so every minute is present
#' data_nelson_pad <- time_pad(data_nelson, interval = "min", round = "day")
#' 
#' # Keep identifying variables "site" and "sensor"
#' data_ozone_sensor_pad <- time_pad(data_ozone_sensor, interval = "hour", 
#'   by = c("site", "sensor"))
#' 
#' }
#' 
#' @export
time_pad <- function(df, interval = "hour", by = NA, round = NA, warn = TRUE) {
  
  # Get variable order
  variables <- names(df)
  
  if (is.na(by[1])) {
    
    # No group-by needed
    df <- padder(df, interval, by, round, merge, warn) %>% 
      arrange_left(variables)
    
  } else {
    
    # Get around non-standard evaluation
    list_dots <- lapply(by, as.symbol)
    
    # Pad by group
    df <- df %>% 
      group_by_(.dots = list_dots) %>%
      do(padder(., 
                interval = interval, 
                by = by,
                round = round, 
                warn = warn)) %>% 
      arrange_left(variables)
    
  }
  
  # Drop dplyr's tbl df
  df <- base_df(df)

  # Return 
  df
  
}


# The worker
# 
# No export
padder <- function(df, interval, by, round, merge, warn) {
  
  # Check if input has a date variable
  if (!"date" %in% names(df))
    stop("Data frame must contain a date variable/column and must be named 'date'.",
         call. = FALSE)
  
  # NA check
  if (any(is.na(df$date)))
    stop("'date' must not contain missing (NA) values.", call. = FALSE)
  
  # Check class
  if (!lubridate::is.POSIXt(df$date)) 
    stop("'date' must be a parsed date.", call. = FALSE)
  
  # Get identifiers
  if (!is.na(by[1])) {
    
    # Get identifiers
    data_by <- get_identifiers(df, by = by)
    
    # Drop identifiers
    df <- df[!names(df) %in% by]
    
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
  date_sequence <- data.frame(date = date_sequence)
  
  # Do the padding
  # Use dplyr, it is much faster
  df <- dplyr::left_join(date_sequence, df, by = "date")

  # Overwrite identifiers
  if (!is.na(by[1])) df <- cbind(data_by, df)
  
  # Message
  if (warn) {
    
    if (any(duplicated(df$date))) 
      warning("Duplicated dates detected.", call. = FALSE)
    
  }
  
  # Return
  df
  
}


# Function to get identifiers from a data frame
get_identifiers <- function(df, by) {
  
  # Catch dplyr's table so indexing works as expected
  df <- base_df(df)
  
  if (length(by) == 1) {
    
    # Single row
    data_by <- df[, by][1]
    
    # Data frame building
    data_by <- data.frame(data_by)
    names(data_by) <- by
    
  } else {
    
    # Selecting vector
    by_collapse <- stringr::str_c(
      stringr::str_c("\\b", by, "\\b"), collapse = "|")
    
    # Get first row
    data_by <- df[, grep(by_collapse, names(df))][1, ]
    
    # Reset
    row.names(data_by) <- NULL
    
  }
  
  # Return
  data_by
  
}
