#' Function to transform a data frame into a \strong{zoo} time series object. 
#' 
#' @param df Data frame or tibble containing a parsed date variable named 
#' \code{"date"}. 
#' 
#' @author Stuart K. Grange. 
#' 
#' @return \strong{zoo} series. 
#' 
#' @example
#' 
#' # Example data
#' data_example <- structure(
#' list(
#'   date = structure(c(1238713200, 1238799600, 1238886000, 1238972400, 1239058800),
#'   class = c("POSIXct", "POSIXt"), tzone = "Etc/GMT-1"),
#'   value = c(0.299977143295109, 0.491486074170098, 0.792906971182674, 
#'   0.0756499643903226, 0.00722031807526946)
#' ), 
#' class = "data.frame", row.names = c(NA, -5L)
#' )
#' 
#' # To zoo series, date variable needed
#' zoo_df <- data_frame_to_zoo(data_example)
#' 
#' @export
data_frame_to_zoo <- function(df) {
  
  # Check date variable
  stopifnot("date" %in% names(df))
  stopifnot(lubridate::is.POSIXct(df$date))
  
  # Get date as vector
  date <- pull(df, date)
  
  # Drop date to create a zoo object
  zoo_df <- zoo::zoo(select(df, -date), date)
  
  return(zoo_df)
  
}


#' Function to transform a \strong{zoo} time series object to a tibble. 
#' 
#' @param zoo_df A \strong{zoo} time series object.
#' 
#' @param clean_names Should the names be cleaned? 
#' 
#' @author Stuart K. Grange. 
#' 
#' @return Tibble. 
#' 
#' @export
zoo_to_data_frame <- function(zoo_df, clean_names = FALSE) {
  
  # Check input
  stopifnot(class(zoo_df) == "zoo")
  
  # Get dates, this is the object's index
  date <- zoo::index(zoo_df)
  
  # Get data, add date, and push to tibble
  df <- as.data.frame(zoo_df, check.names = FALSE, stringsAsFactors = FALSE) %>% 
    mutate(date = date) %>% 
    select(date, 
           everything()) %>% 
    as_tibble()
  
  if (clean_names) {
    names(df) <- if_else(names(df) == "(Intercept)", "intercept", names(df))
  }
  
  return(df)
  
}
