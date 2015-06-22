#' Function to pad timeseries.
#'
#' timePad use is analogous to openair::timeAverage, however the aggregation 
#' values does not occur. timePad does not drop non-numerical variables, can 
#' include identifiers post-pad, and can start and end a padded timeseries at a
#' 'nice place'. For example at the beginning of an hour.
#' 
#' timePad pads a timeseries by calculating the maximum and minimum dates within
#' a timeseries and then generating a uniform date sequence between the maximum
#' and minimim dates. This date sequence is then joined to the inputed data 
#' frame and the missing values are represented as NA. 
#' 
#' timePad uses dplyr::left_join for the date join because testing has 
#' demonstrated this function performs much faster than base::merge. However, 
#' users can still opt to use base::merge because dplyr is still reporting many 
#' bugs. 
#'
#' @param x A data frame including parsed dates
#' @param pad.time Frequency of time padding
#' @param start.unit A date unit where the padded timeseries should begin and 
#' end. Examples are 'hour', 'day', 'month', and 'year'. 
#' @param id.var Should identifying variables be applied to the data post-
#' pad? 
#' @param dplyr.join Should dplyr::left_join be used rather than base:merge for
#' the date sequence join? dplyr::left_join performs better
#' @param remove.final Should the final observation of the padded timeseries be
#' removed? Sometimes if makes sense to remove the last observation if the
#' end date has been rounded forwards. 
#' 
#' @author Stuart K. Grange
#' 
#' @examples
#' 
#' # Pad time series so every minute is present
#' \dontrun{
#' data.nelson.pad <- timePad(data.nelson, pad.time = 'minute', 
#' start.unit = 'day')
#' }
#' 
#' @export
#' 
timePad <- function (x, pad.time = '10 min', start.unit = NULL, id.var = NULL, 
                     dplyr.join = TRUE, remove.final = FALSE) {
  
  # Ensure data frame is data frame, issue occur when dplyr::tbl_df is used
  x <- data.frame(x)
  
  # Get identifying vairables
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
    identifiers <- lapply(x[, id.var], first.non.na)
    
    # Drop ids from data frame  
    x <- x[!names(x) %in% id.var]
    
  }
  
  # Find the start and end of the date sequence
  if (is.null(start.unit)) {
    
    # No date rounding, use date values in x
    date.start <- min(x$date)
    date.end <- max(x$date)
    
  } else {
    
    # Date rounding
    date.start <- lubridate::floor_date(min(x$date), start.unit)
    date.end <- lubridate::ceiling_date(max(x$date), start.unit)
    
  }
  
  
  # Create the sequence of dates
  date.sequence <- data.frame(date = seq(date.start, date.end, by = pad.time))
  
  # Do the padding
  if (dplyr.join) {
    
    # dplyr
    x <- dplyr::left_join(date.sequence, x, by = 'date')
    
  } else {
    
    # base::merge
    x <- merge(date.sequence, x, all = TRUE, by = 'date')
    
  }
  
  
  # Add the id variables to the padded data
  if (!is.null(id.var)) {
    
    x <- cbind(x, identifiers)
    
  }
  
  
  # Remove final observation
  if (remove.final) {
    
    x <- x[-nrow(x), ]
    
  }
  
  # Return
  x
  
}
