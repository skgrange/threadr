#' Function to add a variable by testing an identifier and a range. 
#' 
#' @param df Data frame to test. 
#' 
#' @param test Variable in \code{df} for the range test.
#' 
#' @param df_map Mapping data frame containing \code{by}, \code{min}, \code{max},
#' and \code{add} variables. 
#' 
#' @param by An identifying variable (or up to three) in both \code{df} and 
#' \code{df_map} which is to be used for the matching test.
#' 
#' @param min,max Variables in \code{df_map} which will be used for the range
#' test, i.e., is \code{test} between \code{min} and \code{max}? 
#' 
#' @param add Variable in \code{df_map} to add to \code{df}. Generally, this 
#' will be an integer key. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' # Add a site variable based on sensor_id and date ranges to an observation
#' # table containing time series
#' add_by_id_and_range(
#'   data_observations,
#'   test = "date",
#'   df_map = data_site_ranges,
#'   by = "sensor_id",
#'   min = "date_start",
#'   max = "date_end",
#'   add = "site"
#' )
#' 
#' }
#' 
#' @export
add_by_id_and_range <- function(df, test, df_map, by, min, max, add) {
  
  # TODO: make number of `by` generic. How does one dynamically construct the 
  # if_else testing statement? 
  
  # Check inputs
  stopifnot(c(test, by) %in% names(df))
  stopifnot(c(by, add, min, max) %in% names(df_map))
  stopifnot(length(by) <= 3)
  
  # Determine what NA type to use
  na_type <- df_map %>% 
    select(!!add) %>% 
    pull() %>% 
    get_na_type()
  
  # Pre-allocate variable
  df <- mutate(df, !!add := na_type)
  
  # Test and replace
  for (i in seq_len(nrow(df_map))) {
    
    # For when there is only one identifier
    if (length(by) == 1L) {
      
      # Repeatedly mutate in place
      df <- df %>%
        mutate(
          !!add := if_else(
            !!sym(by) == !!df_map[i, by, drop = TRUE] &
              !!sym(test) >= !!df_map[i, min, drop = TRUE] &
              !!sym(test) <= !!df_map[i, max, drop = TRUE],
            !!df_map[i, add, drop = TRUE],
            !!sym(add)
          )
        )
      
      # For when there are two identifiers
    } else if (length(by) == 2L) {
      
      # Repeatedly mutate in place
      df <- df %>%
        mutate(
          !!add := if_else(
            !!sym(by[1]) == !!df_map[i, by[1], drop = TRUE] &
              !!sym(by[2]) == !!df_map[i, by[2], drop = TRUE] &
              !!sym(test) >= !!df_map[i, min, drop = TRUE] &
              !!sym(test) <= !!df_map[i, max, drop = TRUE],
            !!df_map[i, add, drop = TRUE],
            !!sym(add)
          )
        )
      
      # For when there are three identifiers 
    } else if (length(by) == 3L) {
      
      # Repeatedly mutate in place
      df <- df %>%
        mutate(
          !!add := if_else(
            !!sym(by[1]) == !!df_map[i, by[1], drop = TRUE] &
              !!sym(by[2]) == !!df_map[i, by[2], drop = TRUE] &
              !!sym(by[3]) == !!df_map[i, by[3], drop = TRUE] &
              !!sym(test) >= !!df_map[i, min, drop = TRUE] &
              !!sym(test) <= !!df_map[i, max, drop = TRUE],
            !!df_map[i, add, drop = TRUE],
            !!sym(add)
          )
        )
      
    }
    
  }
  
  return(df)
  
}


get_na_type <- function(x) {
  
  if (is.logical(x)) {
    na_type <- as.logical(NA)
  } else if (is.integer(x)) {
    na_type <- as.integer(NA)
  } else if (is.double(x) && !lubridate::is.POSIXct(x)) {
    na_type <- as.numeric(NA)
  } else if (is.character(x)) {
    na_type <- as.character(NA)
  } else if (is.factor(x)) {
    na_type <- as.factor(NA)
  } else if (lubridate::is.POSIXct(x)) {
    na_type <- lubridate::NA_POSIXct_
  }
  
  return(na_type)
  
}
