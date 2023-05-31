#' Function to aggregate time series data by dates.
#' 
#' \code{aggregate_by_date} does a similar job and has the same objectives of
#' \strong{openair}'s \code{timeAverage}. However, it has been developed 
#' to perform on "longer" data which is often encountered. 
#' 
#' @param df Input data frame to be aggregated. \code{df} must contain 
#' \code{"date"} and \code{"value"} variables. the \code{"date"} variable 
#' must be a \code{POSIXct} date class and the value must be a \code{numeric} or
#' \code{integer} data type. 
#' 
#' @param interval What interval should the aggregation be? Default is 
#' \code{"hour"}.
#' 
#' @param by What variables should \code{df} be grouped by? Common groups are 
#' \code{"site"} and \code{"variable"}. 
#' 
#' @param summary What summary function should be applied for the aggregation? 
#' Default is the \code{mean}. The options are: 
#' 
#' \itemize{
#'   \item{mean}
#'   \item{median}
#'   \item{max}
#'   \item{min}
#'   \item{sum}
#'   \item{count}
#'   \item{sd}
#'   \item{mode}
#'   \item{data_capture}
#' }
#' 
#' @param threshold What data capture threshold is needed to create a valid 
#' aggregation. This is an value between \code{0} and \code{1}. Zero would mean 
#' any number of values will be valid but \code{0.75} would mean \code{75 \%} of
#' values are needed for a valid average. 
#' 
#' @param round Should the aggregations be rounded? Default is no but \code{3} 
#' would round to three decimal places. 
#' 
#' @param pad Should the time series be padded before aggregation? Almost always
#' this will be \code{TRUE} but if you want to speed the function up and have done
#' this previously, it can be set to \code{FALSE}. 
#' 
#' @param determine_interval Should the input time series be evaluate to find
#' it's averaging period/interval. This is required for the correct calculation 
#' of \code{threshold}. 
#' 
#' @param warn Should the function return warnings in certain situations? 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @return Tibble.
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \code{\link[openair]{timeAverage}}, \code{\link{time_pad}}
#' 
#' @examples 
#' \dontrun{
#' 
#' # Aggregate to hourly means
#' aggregate_by_date(data_air, "hour", by = c("site", "variable"))
#' 
#' }
#' 
#' @export
aggregate_by_date <- function(df, interval = "hour", by = NA, summary = "mean", 
                              threshold = 0, round = NA, pad = TRUE, 
                              determine_interval = TRUE, warn = TRUE,
                              verbose = FALSE) {
  
  # Check the input
  # The needed variables
  if (!any(c("date", "value") %in% names(df))) {
    cli::cli_abort("Input must contain `date` and `value` variables.")
  }
  
  # Check data type
  if (!class(df$value) %in% c("numeric", "integer")) {
    cli::cli_abort("`value` must be of numeric or integer class.")
  }
  
  if (!threshold <= 1 & threshold >= 0) {
    cli::cli_abort("`threshold` must be between 0 and 1.")
  }
  
  # Return empty data frame if input is empty
  if (nrow(df) == 0) {
    if (warn) {
      cli::cli_warn("Input contains no observations, returning emmpty tibble...")
    }
    return(tibble())
  }
  
  # Threshold means nothing for data capture
  if (summary == "data_capture") {
    threshold <- 0
  }
  
  # Pad time series first, can be a slow process, but is needed for data capture
  if (pad) {
    
    if (verbose) {
      cli::cli_alert_info("{cli_date()} Padding time series...")
    }
    
    # Determine interval
    if (determine_interval) {
      
      if (verbose) {
        cli::cli_alert_info("{cli_date()} Detecting input averaging period/interval...")
      }
      
      interval_of_input <- detect_date_interval(df$date, text_return = TRUE)
      
      # For sequence date generator, needs a specific format
      interval_of_input <- dplyr::case_when(
        interval_of_input == "five_minute" ~ "5 min",
        interval_of_input == "ten_minute" ~ "10 min",
        interval_of_input == "fifteen_minute" ~ "15 min",
        interval_of_input == "half_hour" ~ "30 min",
        TRUE ~ interval_of_input
      )
      
      # Switch for default
      if (interval_of_input == "unknown") {
        if (verbose) {
          cli::cli_alert_info("{cli_date()} Input averaging period/interval could not be determined...")
        }
        interval_of_input <- interval
      }
      
    } else {
      interval_of_input <- interval
    }
    
    # Pad the time series
    df <- time_pad(
      df, 
      interval = interval_of_input, 
      by = by, 
      full = TRUE, 
      warn = FALSE,
      round = interval
    )
    
  }
  
  # Round dates
  df <- mutate(df, date = lubridate::floor_date(date, unit = interval))
  
  # Create groups if needed
  if (is.na(by[1])) {
    by <- "date"
  } else {
    by <- c("date", by)
  }
  
  # Group data frame with a character vector
  df <- group_by(df, across(dplyr::all_of(by)))
  
  # When the mean is desired (the normal use), wind direction needs additional 
  # processing, and the logic used more than once
  to_process_wd <- if_else(
    summary == "mean" && "variable" %in% names(df) && "wd" %in% unique(df$variable), 
    TRUE, FALSE
  )
  
  if (to_process_wd) {
    
    if (verbose) {
      cli::cli_alert_info("{cli_date()} Wind direction (`wd`) detected...")
    }
    
    # Get wind direction
    df_wd <- filter(df, variable == "wd")
    
    # Drop from original data frame
    df <- filter(df, variable != "wd")
    
    # Do the wind direction aggregation
    # Warnings come from max is used when all elements are NA
    suppressWarnings(
      df_wd <- df_wd %>% 
        summarise(
          value = aggregate_by_date_worker(
            value, 
            summary = !!summary, 
            threshold = !!threshold, 
            wd = TRUE
          ),
          .groups = "drop"
        )
    )
    
  }
  
  # Other variables
  if (verbose) {
    cli::cli_alert_info("{cli_date()} Aggregating by date...")
  }
  
  # Warnings come from max is used when all elements are NA
  suppressWarnings(
    df <- df %>% 
      summarise(
        value = aggregate_by_date_worker(
          value, 
          summary = !!summary, 
          threshold = !!threshold, 
          wd = FALSE
        ),
        .groups = "drop"
      )
  )

  # Bind wind direction too
  if (to_process_wd) {
    df <- bind_rows(df, df_wd)
  }
  
  if (verbose) {
    cli::cli_alert_info("{cli_date()} Final clean-up and arranging..")
  }
  
  # Add date end
  df <- df %>% 
    mutate(
      date_end = lubridate::ceiling_date(
        date, 
        unit = interval, 
        change_on_boundary = TRUE
      ),
      date_end = date_end - 1
    )
  
  # Do some post aggregation cleaning
  # Fix the variable order
  if (identical(by, "date")) {
    variable_order <- c("date", "date_end", "value")
  } else {
    variable_order <- c("date", "date_end", by, "value")
  }
  
  # Format tibble, arrange variables and observations
  df <- df %>% 
    select(!!variable_order) %>%
    dplyr::arrange_at(rev(by))
  
  # Round value if desired
  if (!is.na(round)) {
    df <- mutate(df, value = round(value, digits = round))
  }
  
  return(df)
  
}


aggregate_by_date_worker <- function(x, summary, threshold, wd = FALSE) {
  
  if (wd) {
    
    if (threshold == 0) {
      # Use trigonometry
      x <- mean_wd(x, na.rm = TRUE)
    } else {
      # Calculate data capture
      data_capture <- calculate_data_capture(x)
      if (threshold <= data_capture) {
        x <- mean_wd(x, na.rm = TRUE)
      } else {
        # Invalid summary
        x <- NA
      }
    }
    
  } else {
    
    # Get function to use
    aggregation_function <- aggregation_function_type(summary)
    
    if (threshold == 0) {
      x <- aggregation_function(x, na.rm = TRUE)
    } else {
      # Calculate data capture
      data_capture <- calculate_data_capture(x)
      if (threshold <= data_capture) {
        x <- aggregation_function(x, na.rm = TRUE)
      } else {
        # Invalid summary
        x <- NA
      }
    }
    
  }
  
  return(x)
  
}


calculate_data_capture <- function(x) {
  
  # Calculate data capture
  count_all <- length(x)
  count_valid <- sum(!is.na(x))
  data_capture <- count_valid / count_all
  return(data_capture)
  
}


aggregation_function_type <- function(type) {
  
  # Switch
  if (type == "mean") f <- mean
  if (type == "median") f <- median
  if (type %in% c("max", "maximum")) f <- max
  if (type %in% c("min", "minumum")) f <- min
  if (type == "sum") f <- sum_custom
  if (type %in% c("sd", "stdev", "standard_deviation")) f <- sd
  if (type == "mode") f <- mode_average
  # Parse na.rm for consistency, but is is not used
  if (type %in% c("count", "n")) f <- function(x, na.rm) sum(!is.na(x))
  # na.rm is not used here either, this could be wrong if date is not padded
  if (type == "data_capture") {
    f <- function(x, na.rm) sum(!is.na(x)) / length(x)
  }
  
  return(f)
  
}


# If the entire vector is NA, return NA, not 0, usually used for rainfall data
sum_custom <- function(x, na.rm) {
  
  if (all(is.na(x))) {
    x <- NA
  } else {
    x <- sum(x, na.rm = na.rm)
  }
  
  return(x)
  
}
