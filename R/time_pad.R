#' Function to pad time series.
#'
#' \code{time_pad} use is similar to \strong{openair}'s \code{timeAverage}, but
#' the aggregation of values does not occur. \code{time_pad} does not drop 
#' non-numerical variables, can include identifiers post-pad, and can start and
#' end a padded time-series at a "nice place", for example, at the beginning of
#' an hour or day. 
#' 
#' \code{time_pad} pads a time series by calculating the maximum and minimum 
#' dates within a time series and then generating a uniform date sequence 
#' between the maximum and minimum dates. This date sequence is then joined to 
#' the input data frame and the missing values are represented as \code{NA}. 
#' 
#' @param df A tibble/data frame including parsed dates. The date 
#' variable/column must be named \code{date}.
#' 
#' @param interval Interval of returned time series. Some examples could be: 
#' \code{"min"} \code{"hour"}, \code{"day"}, \code{"month"}, \code{"year"} but 
#' multiples such as \code{"5 min"} work too. \code{interval} can also be a 
#' numeric value such as \code{0.5} which is useful for sub-second padding. 
#' 
#' @param by Should \code{time_pad} apply the padding function to groups within
#' \code{df}? This is helpful when there are many sites/other identifiers within
#' \code{df} which need to be padded individually. 
#' 
#' @param round What date-unit should the first and last observations be rounded
#' to? This allows the padded time-series to begin and end at a "nice place". 
#' Examples are \code{"hour"}, \code{"day"}, \code{"month"}, and \code{"year"}.
#' 
#' @param full Should the date joining use the \code{full_join} function? If 
#' \code{TRUE}, no input dates will be lost but the default is \code{FALSE}. 
#' 
#' @param uniform_padding If a \code{by} vector is supplied, should all groups
#' be padded with the same start and end dates? The minimum and maximum dates
#' contained in \code{df} will be used for this uniform padding process. 
#' 
#' @param warn Should the function give a warning when dates are duplicated? 
#' Default is \code{TRUE}. 
#' 
#' @seealso See \code{\link{round_date_interval}}, \code{timeAverage}, 
#' \code{\link{round_date}}, \code{\link{left_join}}, 
#' \code{\link{aggregate_by_date}}
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' # Pad time series so every minute is present
#' data_nelson_pad <- time_pad(data_nelson, interval = "min", round = "day")
#' 
#' # Keep identifying variables "site" and "sensor"
#' data_ozone_sensor_pad <- time_pad(
#'   data_ozone_sensor, 
#'   interval = "hour", 
#'   by = c("site", "sensor")
#' )
#' 
#' }
#' 
#' @export
time_pad <- function(df, interval = "hour", by = NA, round = NA, full = FALSE, 
                     uniform_padding = FALSE, warn = TRUE) {
  
  # Check input
  if (nrow(df) == 0L) {
    cli::cli_abort("Input data frame has no observations.")
  }
  
  # Check if input has a date variable
  if (!"date" %in% names(df)) {
    cli::cli_abort(
      "Input must contain a date variable/column and must be named `date`."
    )
  }
  
  # Missing-ness test
  if (any(is.na(df$date))) {
    cli::cli_abort("`date` must not contain missing (`NA`) values.")
  }
  
  # Check class of date too
  if (!lubridate::is.POSIXct(df$date)) {
    cli::cli_abort("`date` must be a POSIXct date.")
  }
  
  # Clean an argument a bit
  interval <- interval %>% 
    stringr::str_to_lower() %>% 
    stringr::str_trim()
  
  # Switch interval if required
  interval <- dplyr::case_when(
    interval %in% c("second", "seconds") ~ "sec",
    interval %in% c("minute", "minutes") ~ "min",
    interval == "hours" ~ "hour",
    .default = interval
  )
  
  # Attempt to make interval a numeric value for sub-second padding
  interval <- tryCatch({
    as.numeric(interval)
  }, warning = function(w) {
    interval
  })
  
  # When desired, add the global start and end dates so each group will have
  # the same number of observations after padding
  if (!is.na(by[1]) && uniform_padding) {
    
    # Filter to the start and end dates for the entire tibble, not by groups
    df_dates_global <- df %>% 
      filter(date == min(date) | date == max(date)) %>% 
      distinct(date) %>% 
      arrange(date)
    
    # Check if there is only a pair of dates
    if (nrow(df_dates_global) != 2L) {
      cli::cli_warn("There are more than a pair of global start and end dates...")
    }
    
    # Expand global dates with groups (`by`) too
    df_dates_global_expand <- df %>% 
      distinct(across(dplyr::all_of(by))) %>% 
      tidyr::expand_grid(df_dates_global)
    
    # Join the global dates to each of the groups
    df <- dplyr::full_join(df, df_dates_global_expand, by = c("date", by))
    
  }
  
  # For dplyr's grouping
  if (is.na(by[1])) {
    by <- NULL
  }
  
  # Pad by group
  df <- df %>% 
    group_by(across(dplyr::all_of(by))) %>%
    dplyr::group_modify(
      ~time_pad_worker(
        ., 
        interval = interval, 
        by = by,
        round = round, 
        full = full,
        warn = warn
      ),
      .keep = FALSE
    ) %>% 
    ungroup()
  
  return(df)
  
}


time_pad_worker <- function(df, interval, by, round, full, warn) {
  
  # Find the start and end of the date sequence
  if (is.na(round)) {
    # No date rounding, use date values in df
    date_start <- min(df$date)
    date_end <- max(df$date)
  } else {
    # Date rounding
    date_start <- lubridate::floor_date(min(df$date), round)
    date_end <- lubridate::ceiling_date(max(df$date), round)
  }
  
  # Create the sequence of dates
  date_sequence <- seq(date_start, date_end, by = interval)
  
  # Remove final observation if ceiling rounded
  if (!is.na(round)) date_sequence <- date_sequence[-length(date_sequence)]
  
  # Make a tibble with a single date variable
  df_dates <- tibble(date = date_sequence)
  
  # Do the padding
  if (full) {
    df <- df %>% 
      dplyr::full_join(df_dates, ., by = join_by(date)) %>% 
      arrange(date)
  } else {
    df <- left_join(df_dates, df, by = join_by(date))
  }
  
  # Raise a warning if there are duplicated dates
  if (warn && any_duplicated(df$date)) {
    cli::cli_warn("Duplicated dates have been detected...")
  }
  
  return(df)
  
}
