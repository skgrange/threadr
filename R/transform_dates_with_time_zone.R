#' Function to transform potentially heterogeneous time zones and force their
#' representation in the UTC time zone. 
#' 
#' \code{transform_dates_with_time_zone} is useful when time series are 
#' represented in a single time zone, but different groups are better analysed
#' in different time zones. \code{transform_dates_with_time_zone} will return 
#' transformed dates with the UTC time zone, but this may not strictly be true.
#' 
#' @author Stuart K. Grange 
#' 
#' @param df Tibble with \code{date} and \code{time_zone} variables. 
#' 
#' @param warn Should the function raise warnings? 
#' 
#' @return \code{df} with transformed date variables. 
#' 
#' @export
transform_dates_with_time_zone <- function(df, warn = TRUE) {
  
  # Check inputs
  stopifnot(c("date", "time_zone") %in% names(df), lubridate::is.POSIXt(df$date))
  
  # Do the transformations, do each time zone separately
  df <- df %>% 
    dplyr::group_split(time_zone) %>% 
    purrr::map(transform_dates_with_time_zone_worker, warn = warn) %>% 
    purrr::list_rbind() %>% 
    relocate(date_in_time_zone,
             .after = date)
  
  # Also relocate date_end if it exits
  if ("date_end" %in% names(df)) {
    df <- relocate(df, date_end_in_time_zone, .after = date_end)
  }
  
  return(df)
  
}


transform_dates_with_time_zone_worker <- function(df, warn) {
  
  # Get time zone
  time_zone <- unique(df$time_zone)
  
  # Check for uniqueness
  stopifnot(length(time_zone) == 1L)
  
  # Convert dates
  df <- df %>% 
    mutate(date_in_time_zone = lubridate::with_tz(date, tz = time_zone),
           date_in_time_zone = lubridate::force_tz(date_in_time_zone, tz = "UTC"))
  
  # Also convert date_end if the variable exists
  if ("date_end" %in% names(df)) {
    df <- df %>% 
      mutate(
        date_end_in_time_zone = lubridate::with_tz(date_end, tz = time_zone),
        date_end_in_time_zone = lubridate::force_tz(date_end_in_time_zone, tz = "UTC")
      )
  }
  
  # Check for duplicated dates after conversion
  if (warn && any_duplicated(df$date_in_time_zone)) {
    cli::cli_alert_warning("Duplicated dates detected...")
  }
  
  if (warn && "date_end" %in% names(df) && 
      any_duplicated(df$date_end_in_time_zone)) {
    cli::cli_alert_warning("Duplicated `date_end` values detected...")
  }
  
  return(df)
  
}
