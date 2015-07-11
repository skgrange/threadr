#' Function to pad time-series.
#'
#' time_pad use is analogous to openair::timeAverage. However, the aggregation
#' of values does not occur. 
#' 
#' time_pad does not drop non-numerical variables, can include identifiers 
#' post-pad, and can start and end a padded time-series at a "nice place", 
#' for example, at the beginning of an hour or day. 
#' 
#' time_pad pads a time-series by calculating the maximum and minimum dates 
#' within a time-series and then generating a uniform date sequence between the 
#' maximum and minimum dates. This date sequence is then joined to the input 
#' data frame and the missing values are represented as NA. 
#' 
#' time_pad uses dplyr::left_join for the date join because testing has 
#' demonstrated this function performs much faster than base::merge. However, 
#' users can still opt to use base::merge because dplyr is reporting many bugs. 
#'
#' @param df A data frame including parsed dates. The date variable/column must
#' be named 'date'.
#' @param pad.time Frequency of time padding. Some examples could be: "min"
#' "hour", "day", "month", "year" but multiples such as "5 min" work too. 
#' @param start.unit A optional date unit where the padded time-series should 
#' begin and end. Examples are "hour", "day", "month", and "year". 
#' @param id.var Should identifying variables be applied to the data 
#' post-pad? 
#' @param dplyr.join Should dplyr::left_join be used rather than base:merge for
#' the date sequence join? 
#' @param remove.final Should the final observation of the padded time-series be
#' removed? Sometimes if makes sense to remove the last observation if the
#' end date has been rounded forwards. 
#' 
#' @seealso See \code{\link{round_date_interval}}, 
#' \code{\link{openair::timeAverage}}
#' 
#' @author Stuart K. Grange
#' 
#' @examples
#' 
#' # Pad time-series so every minute is present
#' \dontrun{
#' data.nelson.pad <- time_pad(data.nelson, pad.time = "min", start.unit = "day")
#' }
#' 
#' @export
#' 
time_pad <- function (df, pad.time = "10 min", start.unit = NULL, id.var = NULL, 
                     dplyr.join = TRUE, remove.final = FALSE) {
  
  # Ensure data frame is data frame, issue occurs when dplyr::tbl_df is used
  df <- data.frame(df)
  
  # Get identifying variables
  if (!is.null(id.var)) {
    
    # Define function
    first.non.na <- function (vector) {
      
      # Index of na elements
      index <- which(!is.na(vector))
      
      # Get first non-na element
      element <- vector[min(index)]
      
      # Return
      element
      
    }
    
    # Get identifiers
    identifiers <- lapply(df[, id.var], first.non.na)
    
    # Drop ids from data frame  
    df <- df[!names(df) %in% id.var]
    
  }
  
  # Find the start and end of the date sequence
  if (is.null(start.unit)) {
    
    # No date rounding, use date values in df
    date.start <- min(df$date)
    date.end <- max(df$date)
    
  } else {
    
    # Date rounding
    date.start <- lubridate::floor_date(min(df$date), start.unit)
    date.end <- lubridate::ceiling_date(max(df$date), start.unit)
    
  }
  
  # Create the sequence of dates
  date.sequence <- data.frame(date = seq(date.start, date.end, by = pad.time))
  
  # Do the padding
  if (dplyr.join) {
    
    df <- dplyr::left_join(date.sequence, df, by = "date")
    
  } else {
    
    df <- merge(date.sequence, df, all = TRUE, by = "date")
    
  }
  
  # Add the id variables to the padded data
  if (!is.null(id.var)) {
    df <- cbind(df, identifiers)
  }
  
  # Remove final observation
  if (remove.final) {
    df <- df[-nrow(df), ]
  }
  
  # Return
  df
  
}
