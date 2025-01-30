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
#' @param dates_replace Should the function replace/overwrite the original 
#' \code{date} variable(s) with the transformed version? 
#' 
#' @param drop_time_zone Should the helper \code{time_zone} variable be dropped
#' in the return? 
#' 
#' @return \code{df} with transformed date variables. 
#' 
#' @export
transform_dates_with_time_zone <- function(df, dates_replace = TRUE,
                                           drop_time_zone = FALSE) {
  
  # Check inputs
  stopifnot(c("date", "time_zone") %in% names(df), lubridate::is.POSIXt(df$date))
  
  # Do the transformations, do each time zone separately
  df <- df %>% 
    dplyr::group_split(time_zone) %>% 
    purrr::map(
      transform_dates_with_time_zone_worker, dates_replace = dates_replace
    ) %>% 
    purrr::list_rbind()
  
  # Relocate transformed date variables
  if ("date_in_time_zone" %in% names(df)) {
    df <- relocate(df, date_in_time_zone, .after = date)
  }
  
  if ("date_end_in_time_zone" %in% names(df)) {
    df <- relocate(df, date_end_in_time_zone, .after = date_end)
  }
  
  # Drop helper variable too
  if (drop_time_zone) {
    df <- select(df, -time_zone)
  }
  
  return(df)
  
}


transform_dates_with_time_zone_worker <- function(df, dates_replace) {
  
  # Get time zone
  time_zone <- unique(df$time_zone)
  
  # Check for uniqueness
  stopifnot(length(time_zone) == 1L)
  
  # Does the input have a date end too?
  has_date_end <- "date_end" %in% names(df)
  
  # Convert date vectors
  date_transformed <- df %>% 
    pull(date) %>% 
    with_and_force_tz(time_zone)
  
  # Also convert date_end if the variable exists
  if (has_date_end) {
    date_end_transformed <- df %>% 
      pull(date) %>% 
      with_and_force_tz(time_zone)
  }
  
  # Add date vectors to data frame, replace or add
  if (dates_replace) {
    df <- mutate(df, date = !!date_transformed)
    if (has_date_end) {
      df <- mutate(df, date_end = !!date_end_transformed)
    }
  } else {
    df <- mutate(df, date_in_time_zone = !!date_transformed)
    if (has_date_end) {
      df <- mutate(df, date_end_in_time_zone = !!date_end_transformed)
    }
  }
  
  return(df)
  
}


with_and_force_tz <- function(date, time_zone, time_zone_target = "UTC") {
  
  # Convert dates and force time zone after conversion
  date %>% 
    lubridate::with_tz(tzone = time_zone) %>% 
    lubridate::force_tz(tzone = time_zone_target)
  
}
