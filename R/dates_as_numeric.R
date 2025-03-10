#' Function to convert all date variables in a data frame to their numeric 
#' representation. 
#' 
#' @author Stuart K. Grange
#'
#' @param df Data frame or tibble. 
#' 
#' @param include_times Should times (\code{hms}) also be converted? 
#'
#' @return \code{df} with numeric dates and times. 
#'
#' @export
dates_as_numeric <- function(df, include_times = TRUE) {
  
  # Check input
  stopifnot("data.frame" %in% class(df))
  
  # Convert all dates and date times
  df <- mutate(df, across(lubridate::is.POSIXt, as.numeric))
  
  # Convert times too
  if (include_times) {
    df <- mutate(df, across(hms::is_hms, as.numeric))
  }
  
  return(df)
  
}
