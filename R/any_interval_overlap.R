#' Function to test if any intervals overlap with one another. 
#' 
#' @author Stuart K. Grange
#' 
#' @param df Tibble containing at least two variables, \code{"rowid"} and
#' \code{"interval"}. \code{"interval"} must be of data type \code{interval}. 
#' 
#' @return Logical vector, length of 1. 
#' 
#' @export
any_interval_overlap <- function(df) {
  
  # Check the inputs
  stopifnot(all(c("rowid", "interval") %in% names(df)))
  stopifnot(inherits(df$interval, "Interval"))
  
  # Make all combinations and test
  df_tests <- tidyr::expand_grid(
    select(df, x_id = rowid, x_interval = interval),
    select(df, y_id = rowid, y_interval = interval)
  ) %>% 
    filter(x_id != y_id) %>% 
    mutate(interval_overlaps = lubridate::int_overlaps(x_interval, y_interval))
  
  # Collapse into a logical with length of 1
  x <- any(df_tests$interval_overlaps)
  
  return(x)
  
}
