#' Function to test if a date is within a vector of intervals. 
#' 
#' @author Stuart K. Grange
#' 
#' @param df Input data frame. 
#' 
#' @param df_look Invalidation data frame containing \code{"date_start"} and 
#' \code{"date_end"} variables.
#' 
#' @return A logical vector.
#' 
#' @examples 
#' \dontrun{
#' # Load invalidation table and parse dates
#' data_invalid <- read.csv("weather_underground_invalidation_table.csv") %>% 
#'   mutate(date_start = ymd_hm(date_start),
#'          date_end = ymd_hm(date_end))
#'          
#' # Test dates
#' data_nelson$interval_test <- interval_test(data_nelson, data_invalid)
#' 
#' }
#' 
#' @export
interval_test <- function (df, df_look) {
  
  # Check input data
  if (!all(c("date_start", "date_end") %in% names(df_look)))
    stop("Invalidation table needs to contain 'date_start' and 'date_end' variables.", 
         call = FALSE)
  
  # Get interval
  interval <- lubridate::interval(df_look$date_start, df_look$date_end)
  
  # Check if x is within any interval
  valid <- sapply(df$date, function (x) 
    any(ifelse(x %within% interval, TRUE, FALSE)))
  
  # Return
  valid
  
}
