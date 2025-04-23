#' Function to add Monday-based week number and date ranges to a tibble. 
#' 
#' @author Stuart K. Grange
#' 
#' @param df A tibble/data frame including parsed dates. The date 
#' variable/column must be named \code{date}.
#' 
#' @return \code{df} with four new variables located after the \code{date} 
#' variable.
#' 
#' @export
add_monday_based_weeks <- function(df) {
  
  # Check the input
  if (nrow(df) == 0L) {
    cli::cli_abort("Input data frame has no observations.")
  }
  
  if (!"date" %in% names(df)) {
    cli::cli_abort("Input must contain a date variable/column and must be named `date`.")
  }
  
  if (any(is.na(df$date))) {
    cli::cli_abort("`date` must not contain missing (`NA`) values.")
  }
  
  if (!lubridate::is.POSIXct(df$date)) {
    cli::cli_abort("`date` must be a POSIXct date.")
  }
  
  # Get dates from input
  date <- df %>% 
    pull(date) %>% 
    unique() %>% 
    sort()
  
  # Get time zone from date
  tz <- time_zone(date)
  
  # Get years of input
  years <- unique(lubridate::year(date))
  
  # Create date sequence
  # Get start and end dates
  # Use the past and future years to handle incomplete weeks
  date_start <- lubridate::ymd(stringr::str_c(min(years) - 1, "-01-01"), tz = tz)
  date_end <- lubridate::ymd(stringr::str_c(max(years) + 1, "-12-31"), tz = tz)
  
  # Generate daily sequence
  date_sequence <- seq(date_start, date_end, by = "day")
  
  # Get weekday for each date, 1 is Monday
  weekday_numeric <- lubridate::wday(
    date_sequence, week_start = getOption("lubridate.week.start", 1)
  )
  
  # Get start and end dates
  date_start_week <- date_sequence[weekday_numeric == 1L]
  years_start_week <- lubridate::year(date_start_week)
  date_end_week <- date_start_week + (seconds_in_a_day() * 7) - 1
  
  # Build date range tibble
  df_ranges <- tibble(
    week_monday_year = years_start_week,
    date_start_week = date_start_week,
    date_end_week = date_end_week
  ) %>% 
    group_by(week_monday_year) %>% 
    mutate(week_monday_number = 1L:n()) %>% 
    ungroup() %>% 
    relocate(week_monday_number,
             .after = week_monday_year)
  
  # Join weekly dates to date in tibble
  df_join <- df %>% 
    left_join(
      df_ranges, 
      by = join_by(
        between(date, date_start_week, date_end_week)
      )
    ) %>% 
    relocate(week_monday_year,
             week_monday_number,
             date_start_week,
             date_end_week,
             .after = date)
  
  return(df_join)
  
}
