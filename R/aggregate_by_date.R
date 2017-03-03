#' Function to aggregate time-series data by dates.
#' 
#' \code{aggregate_by_date} does a similar job and has the same objectives of
#' \strong{openair}'s \code{\link{timeAverage}}. However, it has been developed 
#' to perform on "longer" data which is often encountered. 
#' 
#' @param df Input data frame to be aggregated. \code{df} must contain 
#' \code{"date"} and \code{"value"} variables and the \code{"date"} variable 
#' must be a \code{POSIXct} date class. 
#' 
#' @param interval What interval should the aggregation be? Default is 
#' \code{"hour"}.
#' 
#' @param by What variables should \code{df} be grouped by? Common groups are 
#' \code{"site"} and \code{"variable"}. 
#' 
#' @param summary What summary function should be applied for the aggregation? 
#' Default is the \code{mean}. 
#' 
#' @param threshold What data capture threshold is needed to create a valid 
#' aggregation. This is an value between \code{0} and \code{1}. Zero would mean 
#' any number of values will be valid but \code{0.75} would mean \code{75 \%} of
#' values are needed for a valid average. 
#' 
#' @param round Should the aggregations be rounded? Default is no but \code{3} 
#' would round to three decimal places. 
#' 
#' @return Data frame. 
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \code{\link{timeAverage}}, \code{\link{time_pad}}
#' 
#' @import dplyr
#' 
#' @examples 
#' \dontrun{
#' 
#' # Aggregate to hourly means
#' aggregate_by_date(data_air, "hour", by = c("site", "variable"))
#' 
#' }
#' 
#' @export
aggregate_by_date <- function(df, interval = "hour", by = NA, summary = "mean", 
                              threshold = 0, round = NA) {
  
  # Check a few things
  if (!any(c("date", "value") %in% names(df)))
    stop("Input must contain 'date' and 'value' variables.", call. = FALSE)
  
  if (!threshold <= 1 & threshold >= 0) 
    stop("Threshold must be between 0 and 1.", call. = FALSE)
  
  # 
  date_end_addition <- ifelse(interval == "day", 86399, NA)
  date_end_addition <- ifelse(interval == "hour", 3599, date_end_addition)
  
  if (is.na(by)[1]) {
    
    list_dots <- list(as.symbol("date"))
    
  } else {
    
    list_dots <- lapply(c("date", by), as.symbol)
    
  }
  
  # Pad time series first
  df <- time_pad(df, interval = interval, by = by, full = TRUE, round = interval,
                 warn = FALSE)
  
  # Create groups
  df <- df %>% 
    mutate(date = lubridate::floor_date(date, unit = interval)) %>% 
    group_by_(.dots = list_dots)
  
  # Do the aggregation
  df <- df %>% 
    summarise(value = date_aggregator(
      value, summary = summary, threshold = threshold)) %>% 
    ungroup() %>% 
    mutate(date_end = date + date_end_addition) %>% 
    # select_("date",
    #         "date_end",
    #         .dots = list_dots,
    #         "value") %>%
    arrange_(.dots = rev(list_dots))
  
  # Round
  if (!is.na(round)) df$value <- round(df$value, round)
  
  # Return
  df
  
}


date_aggregator <- function(x, summary, threshold) {
  
  if (threshold == 0) {
    
    aggregation <- mean(x, na.rm = TRUE)
    
  } else {
    
    # Calculate data capture
    count_all <- length(x)
    count_valid <- sum(!is.na(x))
    data_capture <- count_valid / count_all
    
    if (threshold <= data_capture) {
      
      aggregation <- mean(x, na.rm = TRUE)
      
    } else {
      
      # Invalid summary
      aggregation <- NA
      
    }
    
  }
  
  # Return
  aggregation
  
}


# wind_direction_averager <- function(x) {
#   
#   
#   
# }
