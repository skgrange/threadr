#' Function to calculate summaries of dates for a continuous time series when
#' the data are in "wide" format. 
#' 
#' \code{calculate_date_summaries_wide} will pad the time series before 
#' aggregation to ensure all date steps are present in the return. 
#' 
#' @author Stuart K. Grange
#' 
#' @param df Input tibble. \code{df} must contain a \code{date} variable and at
#' least one variable with a a \code{numeric} data type. 
#' 
#' @param ... Grouping variables/specification for the summaries.
#' 
#' @param interval What interval should the aggregation be? English names and 
#' multiples of time intervals are accepted. For example, a valid \code{interval}
#' is \code{"5 mins"}. 
#' 
#' @param use_data_table Should the \code{data.table} backend be used for the
#' aggregation calculations? 
#' 
#' @param progress Should a progress bar be displayed? 
#' 
#' @return Tibble. 
#' 
#' @seealso \code{\link{calculate_date_summaries}}
#'
#' @export
calculate_date_summaries_wide <- function(df, ..., interval = "hour",
                                          use_data_table = TRUE,
                                          progress = FALSE) {
  
  # Check the inputs
  stopifnot("date" %in% names(df) && lubridate::is.POSIXct(df$date))
  
  # No missing dates are allowed
  if (anyNA(df$date)) {
    cli::cli_abort("Missing dates detected.")
  }
  
  # Switch interval if needed
  interval <- dplyr::case_when(
    interval == "minute" ~ "min",
    .default = interval
  ) %>% 
    stringr::str_to_lower()
  
  # Catch the n arguments that are used for grouping the tibble, called a defuse
  # operation
  dots <- dplyr::enquos(..., .named = TRUE)
  
  # Nest the tibble
  df_nest <- df %>% 
    group_by(!!!dots) %>% 
    dplyr::nest_by(.key = "observations", 
                   .keep = FALSE)
  
  # Pull the nested tibbles
  list_agg <- pull(df_nest, observations)
  
  # Do the time padding and aggregation across n variables/columns
  list_agg <- list_agg %>% 
    purrr::map(
      ~calculate_date_summaries_wide_worker(
        ., interval = interval, use_data_table = use_data_table
      ), 
      .progress = progress
    )
  
  # Add to aggregated tibbles to nested object and reframe to keep the 
  # identifiers
  df_agg <- df_nest %>% 
    mutate(observations = !!list_agg) %>% 
    dplyr::reframe(observations)
  
  return(df_agg)
  
}


calculate_date_summaries_wide_worker <- function(df, interval, use_data_table) {
  
  # Get date range
  date_range <- range(df$date)
  
  # Round to nearest date boundaries
  date_start <- lubridate::floor_date(date_range[1], interval)
  date_end <- lubridate::ceiling_date(date_range[2], interval)
  
  # Create a sequence with all dates present, used for padding the time series
  date_sequence <- seq(date_start, date_end, by = interval)
  
  # Drop final element of sequence because of the ceiling date rounding
  date_sequence <- head(date_sequence, -1)
  
  # Create a tibble for joining
  df_date_sequence <- tibble(date = date_sequence)
  
  # Join the observations to the date sequence tibble, the padding operation
  df_join <- dplyr::full_join(df_date_sequence, df, by = join_by(date))
  
  # Add date grouping
  df_join <- df_join %>% 
    mutate(date = lubridate::floor_date(date, interval)) %>% 
    group_by(date)
  
  # Get numeric variable names, required for the data.table call
  variables_numeric <- names(df_join)[purrr::map_lgl(df_join, is.numeric)]
  
  # To a data.table data frame to do the aggregation if desired
  if (use_data_table) {
    df_join <- dtplyr::lazy_dt(df_join)
  }
  
  # Do the aggregation across all numeric variables and add date_end too
  df_agg <- df_join %>% 
    summarise(across(tidyselect::all_of(variables_numeric), ~mean(., na.rm = TRUE)),
              .groups = "drop") %>% 
    mutate(
      date_end = lubridate::ceiling_date(
        date, unit = interval, change_on_boundary = TRUE
      ) - 1
    ) %>% 
    relocate(date_end, 
             .after = date) %>% 
    as_tibble()
  
  return(df_agg)
  
}
