#' Function to pad time-series.
#'
#' \code{time_pad} use is similar to \code{openair::timeAverage}. However, the
#' aggregation of values does not occur. 
#' 
#' \code{time_pad} does not drop non-numerical variables, can include 
#' identifiers post-pad, and can start and end a padded time-series at a 
#' "nice place", for example, at the beginning of an hour or day. 
#' 
#' \code{time_pad} pads a time-series by calculating the maximum and minimum 
#' dates within a time-series and then generating a uniform date sequence 
#' between the maximum and minimum dates. This date sequence is then joined to 
#' the input data frame and the missing values are represented as \code{NA}. 
#' 
#' To-do: Enhance group-by operations by using \code{dplyr} rather than 
#' \code{plyr}. 
#'
#' @param df A data frame including parsed dates. The date variable/column must
#' be named \code{date}.
#' @param interval Interval of returned time series. Some examples could be: 
#' "min" "hour", "day", "month", "year" but multiples such as "5 min" work too. 
#' @param by Should \code{time_pad} apply the padding function to groups within
#' \code{df}? This is helpful when there are many sites/other identifiers within
#' \code{df} which need to be padded individually. 
#' @param round An optional date-unit to round the first and last observations
#' of \code{df}. This allows the padded time-series to begin and end at a 
#' "nice place". Examples are "hour", "day", "month", and "year". 
#' @param id What identifying variables should be applied to the data post-pad? 
#' \code{id} can take multiple values. 
#' @param final Should the final observation of the padded time-series be
#' kept? Sometimes if makes sense to remove the last observation if the
#' end-date has been rounded forwards. 
#' 
#' @seealso See \code{\link{round_date_interval}}, \code{\link{timeAverage}}, 
#' \code{\link{round_date}}, \code{\link{left_join}}
#' 
#' @author Stuart K. Grange
#' 
#' @examples
#' 
#' \dontrun{
#' # Pad time-series so every minute is present
#' data.nelson.pad <- time_pad(data.nelson, interval = "min", round = "day")
#' 
#' # Keep identifying variables "site" and "sensor"
#' data.ozone.sensor.pad <- time_pad(data.ozone.sensor, interval = "hour", 
#'   id = c("site", "sensor"))
#' 
#' }
#' 
#' @export
#' 
time_pad <- function (df, interval = "hour", by = NA, round = NA, final = TRUE) {
  
  # Ensure data frame is data frame, issues occurs when dplyr::tbl_df is used
  # due to the lack of indices
  df <- base_df(df)
  
  if (!is.na(by)[1]) {
    
    # Group-by operation
    df <- plyr::ddply(df, as.quoted(by), padder, interval = interval, id = by, 
                      round = round, final = final)
    
    # To-do...
    #     df <- df %>% 
    #       group_by_(by) %>% 
    #       padder(interval = interval, id = by, round = round, final = final)) %>% 
    #       ungroup()
    
  } else {
    
    # No grouping so no need to use dplyr
    df <- padder(df, interval = interval, id = by, round = round, final = final)
    
  }
  
  # Return 
  df
  
}


# The main function
# No export
#
padder <- function (df, interval, id = NA, round = NA, final = TRUE) {
  
  # Check if df has a date variable
  if (!"date" %in% names(df)) {
    stop("Input data frame must contain a date variable/column and must be named 'date'")
  }
  
  # Get identifying variables
  if (!is.na(id[1])) {
    
    if (length(id) == 1) {
      
      identifiers <- first_na_element(df[, id])
      identifiers <- data.frame(identifiers)
      names(identifiers) <- id
      
    } else {
      
      # Get the first non-NA elements
      identifiers <- lapply(df[, id], first_na_element)
      # Make a data frame
      identifiers <- data.frame(identifiers)
      
    }
    
    # Drop ids from data frame  
    df <- df[!names(df) %in% id]
    
  }
  
  # Find the start and end of the date sequence
  if (is.na(round)) {
    
    # No date rounding, use date values in df
    date.start <- min(df$date)
    date.end <- max(df$date)
    
  } else {
    
    # Date rounding
    date.start <- lubridate::floor_date(min(df$date), round)
    date.end <- lubridate::ceiling_date(max(df$date), round)
    
  }
  
  # Create the sequence of dates
  date.sequence <- data.frame(date = seq(date.start, date.end, by = interval))
  
  # Do the padding
  df <- dplyr::left_join(date.sequence, df, by = "date")
  
  # Add the id variables to the padded data
  if (!is.na(id[1])) {
    # cbind will replicate/recycle the vector
    df <- cbind(df, identifiers)
  }
  
  # Remove final observation
  if (!final) {
    df <- df[-nrow(df), ]
  }
  
  # Return
  df
  
}

# Define function to find first non-NA element
first_na_element <- function (vector) {
  
  # Index of na elements
  index <- which(!is.na(vector))
  index <- min(index)
  
  # Get first non-na element
  element <- vector[index]
  
  # Return
  element
  
}
