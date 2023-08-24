#' Function to calculate summaries of dates for a continuous time series. 
#' 
#' \code{calculate_date_summaries} will pad the time series before aggregation
#' to ensure all date steps are present in the return. 
#' 
#' @author Stuart K. Grange
#' 
#' @param df Input tibble. \code{df} must contain \code{date} and \code{value} 
#' variables. The \code{date} variable must be a \code{POSIXct} date class and
#' the value must be a \code{numeric} data type. 
#' 
#' @param ... Grouping variables/specification for the summaries. If location
#' and variable variables are contained in \code{df}, these will usually be 
#' used as groups. If a column/variable is used, wind direction (\code{wd}) will
#' be processed with vector functions. 
#' 
#' @param interval What interval should the aggregation be? English names and 
#' multiples of time intervals are accepted. For example, a valid \code{interval}
#' is \code{"5 mins"}. 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @param progress Should a progress bar be displayed? 
#' 
#' @return Tibble. 
#' 
#' @seealso \code{\link{aggregate_by_date}}
#' 
#' @export
calculate_date_summaries <- function(df, ..., interval = "hour", verbose = FALSE,
                                     progress = FALSE) {
  
  # Check the inputs
  stopifnot("date" %in% names(df) && lubridate::is.POSIXct(df$date))
  stopifnot("value" %in% names(df) && inherits(df$value, "numeric"))
  
  # Catch the n arguments that are used for grouping the tibble, called a defuse
  # operation
  dots <- dplyr::enquos(..., .named = TRUE)
  
  # Has a variable been passed? 
  has_variable <- "variable" %in% names(dots)
  
  # Nest the tibble
  df_nest <- df %>% 
    group_by(!!!dots) %>% 
    dplyr::nest_by(.key = "observations",
                   .keep = FALSE)
  
  # Print a message
  if (verbose) {
    cli::cli_alert_info("{cli_date()} `{nrow(df_nest)}` group{?s} to summarise...")
  }

  # Pull the nested tibbles
  list_agg <- pull(df_nest, observations)
  
  # Add variable name because it is used for some logic in the worker
  if (has_variable) {
    list_agg <- purrr::set_names(list_agg, df_nest$variable)
  }
  
  # Do the aggregation
  list_agg <- list_agg %>%
    purrr::imap(
      ~calculate_date_summaries_worker(
        .x, .y, interval = interval, verbose = verbose
      ),
      .progress = progress
    )
  
  # Add to aggregated tibbles to nested object and reframe to keep the 
  # identifiers
  df_nest_agg <- df_nest %>% 
    mutate(observations = !!list_agg) %>% 
    dplyr::reframe(observations)
  
  return(df_nest_agg)
  
}


calculate_date_summaries_worker <- function(df, name, interval, verbose) {
  
  # Switch some intervals if needed
  interval <- dplyr::case_when(
    interval == "minute" ~ "min",
    .default = interval
  ) %>% 
    stringr::str_to_lower()
  
  # Get date range
  date_range <- range(df$date)
  
  # Round to nearest date boundaries
  date_start <- lubridate::floor_date(date_range[1], interval)
  date_end <- lubridate::ceiling_date(date_range[2], interval)
  
  # Create a sequence with all dates present, used for padding the time series
  date_sequence <- seq(date_start, date_end, by = interval)
  
  # Create a tibble for joining
  df_date_sequence <- tibble(date = date_sequence)
  
  # Join the observations to the date sequence tibble, the padding operation
  df_join <- dplyr::full_join(df_date_sequence, df, by = join_by(date))
  
  # Group the tibble
  df <- df_join %>% 
    mutate(date = lubridate::floor_date(date, interval)) %>% 
    group_by(date)
  
  if (name == "wd") {
    
    # Calculate average wind direction
    if (verbose) {
      cli::cli_alert_info(
        "{cli_date()} Wind direction (`wd`) variable detected, calculating average wd..."
      )
    }
    
    # Use the correct logic for wind direction
    df <- df %>% 
      summarise(n = sum(!is.na(value)),
                value = mean_wd(value, na.rm = TRUE),
                .groups = "drop")
    
  } else {
    # Standard use, the mean
    df <- df %>% 
      summarise(n = sum(!is.na(value)),
                value = mean(value, na.rm = TRUE),
                .groups = "drop")
  }
  
  # Calculate and add `date_end`
  df <- df %>% 
    mutate(
      date_end = lubridate::ceiling_date(
        date, unit = interval, change_on_boundary = TRUE
      ) - 1
    ) %>% 
    relocate(date,
             date_end)
  
  return(df)
  
}
