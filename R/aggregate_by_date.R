#' Function to aggregate time-series data by dates.
#' 
#' \code{aggregate_by_date} does a similar job and has the same objectives of
#' \strong{openair}'s \code{timeAverage}. However, it has been developed 
#' to perform on "longer" data which is often encountered. 
#' 
#' @param df Input data frame to be aggregated. \code{df} must contain 
#' \code{"date"} and \code{"value"} variables. the \code{"date"} variable 
#' must be a \code{POSIXct} date class and the value must be a \code{numeric} or
#' \code{integer} data type. 
#' 
#' @param interval What interval should the aggregation be? Default is 
#' \code{"hour"}.
#' 
#' @param by What variables should \code{df} be grouped by? Common groups are 
#' \code{"site"} and \code{"variable"}. 
#' 
#' @param summary What summary function should be applied for the aggregation? 
#' Default is the \code{mean}. The options are: 
#' 
#' \itemize{
#'   \item{mean}
#'   \item{median}
#'   \item{max}
#'   \item{min}
#'   \item{sum}
#'   \item{count}
#'   \item{sd}
#'   \item{mode}
#' }
#' 
#' @param threshold What data capture threshold is needed to create a valid 
#' aggregation. This is an value between \code{0} and \code{1}. Zero would mean 
#' any number of values will be valid but \code{0.75} would mean \code{75 \%} of
#' values are needed for a valid average. 
#' 
#' @param round Should the aggregations be rounded? Default is no but \code{3} 
#' would round to three decimal places. 
#' 
#' @param pad Should the time-series be padded before aggregation? Almost always
#' this will be \code{TRUE} but if you want to speed the function up and have done
#' this previously, it can be set to \code{FALSE}. 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @return Data frame. 
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \code{timeAverage}, \code{\link{time_pad}}
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
                              threshold = 0, round = NA, pad = TRUE, 
                              verbose = FALSE) {
  
  # Check a few things
  if (!any(c("date", "value") %in% names(df)))
    stop("Input must contain 'date' and 'value' variables.", call. = FALSE)
  
  # Check data type
  if (!class(df$value) %in% c("numeric", "integer"))
    stop("'value' must be of numeric or integer class.", call. = FALSE)
  
  if (!threshold <= 1 & threshold >= 0) 
    stop("Threshold must be between 0 and 1.", call. = FALSE)
  
  # For grouping
  if (is.na(by)[1]) {
    
    list_dots <- list(as.symbol("date"))
    
  } else {
    
    list_dots <- lapply(c("date", by), as.symbol)
    
  }
  
  # Threshold means nothing for data capture
  if (summary == "data_capture") threshold <- 0
  
  # Pad time series first, can be the bottle-neck but is needed for data capture
  if (pad) {
    
    if (verbose) message("Padding time-series...")
    
    df <- time_pad(
      df, 
      interval = interval, 
      by = by, 
      full = TRUE, 
      warn = FALSE,
      round = interval
    )
    
  }
  
  # Create groups
  df <- df %>% 
    mutate(date = lubridate::floor_date(date, unit = interval)) %>% 
    dplyr::group_by_(.dots = list_dots)
  
  # Wind direction processing, logic used more than once
  # Double && will break out of test if FALSE, no warnings
  wind_direction_detected <- ifelse(
    "variable" %in% names(df) && "wd" %in% unique(df$variable), 
    TRUE, 
    FALSE
  )
  
  if (wind_direction_detected) {
    
    if (verbose) message("Wind direction ('wd') detected...")
    
    # Get wind direction
    df_wd <- df %>% 
      filter(variable == "wd")
    
    # Drop from orignal data frame
    df <- df %>% 
      filter(variable != "wd")
    
    # Do the aggregation, non-standard evaluation was a mess, but better now
    
    # Do the wind direction aggregation
    df_wd <- df_wd %>% 
      dplyr::summarise(
        value = date_aggregator(
          value, 
          summary = !!summary, 
          threshold = !!threshold, 
          wd = TRUE
        )
      )
    
  }
  
  # Other variables
  if (verbose) message("Aggregating...")
  
  df <- df %>% 
    dplyr::summarise(
      value = date_aggregator(
        value, 
        summary = !!summary, 
        threshold = !!threshold, 
        wd = FALSE
      )
    )
  
  # Bind wind direction too
  if (wind_direction_detected) df <- dplyr::bind_rows(df, df_wd)
  
  if (verbose) message("Final clean-up and arranging...")
  
  # Add date end
  df <- df %>% 
    ungroup() %>% 
    mutate(
      date_end = lubridate::ceiling_date(
        date, 
        unit = interval, 
        change_on_boundary = TRUE
      ),
      date_end = date_end - 1)
  
  # Do some post aggregation cleaning
  
  # Variable order
  names_vector <- c("date", "date_end", by, "value")
  # No NA when by is NA
  names_vector <- names_vector[!is.na(names_vector)]
  
  # Format table
  df <- df %>% 
    dplyr::select_(.dots = names_vector) %>%
    dplyr::arrange_(.dots = rev(list_dots))
  
  # Round
  if (!is.na(round)) df$value <- round(df$value, round)
  
  return(df)
  
}


date_aggregator <- function(x, summary, threshold, wd = FALSE) {
  
  if (wd) {
    
    if (threshold == 0) {
      
      # Use trigonometry
      x <- mean_wd(x, na.rm = TRUE)
      
    } else {
      
      # Calculate data capture
      data_capture <- calculate_data_capture(x)
      
      if (threshold <= data_capture) {
        
        x <- mean_wd(x, na.rm = TRUE)
        
      } else {
        
        # Invalid summary
        x <- NA
        
      }
      
    }
    
  } else {
    
    # Get function to use
    aggregation_function <- aggregation_function_type(summary)
    
    if (threshold == 0) {
      
      x <- aggregation_function(x, na.rm = TRUE)
      
    } else {
      
      # Calculate data capture
      data_capture <- calculate_data_capture(x)
      
      if (threshold <= data_capture) {
        
        x <- aggregation_function(x, na.rm = TRUE)
        
      } else {
        
        # Invalid summary
        x <- NA
        
      }
      
    }
    
  }
  
  return(x)
  
}


calculate_data_capture <- function(x) {
  
  # Calculate data capture
  count_all <- length(x)
  count_valid <- sum(!is.na(x))
  data_capture <- count_valid / count_all
  return(data_capture)
  
}


aggregation_function_type <- function(type) {
  
  # Switch
  if (type == "mean") f <- mean
  if (type == "median") f <- median
  if (type %in% c("max", "maximum")) f <- max
  if (type %in% c("min", "minumum")) f <- min
  if (type == "sum") f <- sum
  if (type %in% c("sd", "stdev", "standard_deviation")) f <- sd
  if (type == "mode") f <- mode_average
  
  # Parse na.rm for consistency, but is is not used
  if (type %in% c("count", "n")) f <- function(x, na.rm) sum(!is.na(x))
  
  # na.rm is not used here either, this could be wrong if date is not padded
  if (type == "data_capture") 
    f <- function(x, na.rm) sum(!is.na(x)) / length(x)
  
  return(f)
  
}
