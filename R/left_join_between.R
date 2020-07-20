#' Function to join two tibbles/data frames together using a between clause. 
#' 
#' \code{left_join_between} is build upon the \strong{fuzzyjoin} package and 
#' \code{\link{findInterval}} function. \code{left_join_between} is also slow. 
#' 
#' @param df,df_y Data frames/tibbles to join. 
#' 
#' @param by What variables to use for the join. 
#' 
#' @param match_fun The functions used for the joining. See examples for clarity. 
#' 
#' @param clean Should the joined output be cleaned before being returned? 
#' 
#' @param drop_between Should the variables used for the between/range test be
#' dropped? 
#' 
#' @return Tibble. 
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \code{fuzzy_left_join}, \code{\link{findInterval}}
#' 
#' @examples
#' 
#' # Build a look-up table
#' data_invalidations <- dplyr::tribble(
#'   ~station_id, ~date_start, ~date_end,
#'   8208, "2019-12-07 16:00", "2019-12-07 17:15"
#' ) %>% 
#'   mutate(valid = FALSE) %>% 
#'   mutate_at(c("date_start", "date_end"), ymd_hms, tz = "UTC", truncated = 3)
#' 
#' # Some time series data
#' data_time_series <- dplyr::tribble(
#'   ~station_id, ~date,                     ~value,
#'   8208L,       "2019-12-07 15:45:00 UTC", 1.753, 
#'   8208L,       "2019-12-07 16:00:00 UTC", 1.745, 
#'   8208L,       "2019-12-07 16:15:00 UTC", 0.075, 
#'   8208L,       "2019-12-07 16:30:00 UTC", 0.076, 
#'   8208L,       "2019-12-07 16:45:00 UTC", 0.075, 
#'   8208L,       "2019-12-07 17:00:00 UTC", 0.076, 
#'   8208L,       "2019-12-07 17:15:00 UTC", 1.701, 
#'   8208L,       "2019-12-07 17:30:00 UTC", 1.692, 
#'   8208L,       "2019-12-07 17:45:00 UTC", 1.683, 
#'   8208L,       "2019-12-07 18:00:00 UTC", 1.674, 
#'   8208L,       "2019-12-07 18:15:00 UTC", 1.668
#' ) %>%
#'   mutate(date = as.POSIXct(date, tz = "UTC"))
#' 
#' # Do the join
#' data_time_series %>% 
#'   left_join_between(
#'   data_invalidations,
#'   by = c(
#'     "station_id" = "station_id",
#'     "date" = "date_start",
#'     "date" = "date_end"
#'   ),
#'   match_fun = list(`==`, `>=`, `<=`)
#' ) 
#' 
#' @export
left_join_between <- function(df, df_y, by, match_fun, clean = TRUE, 
                              drop_between = TRUE) {
  
  # Do the join, can be slow
  df <- fuzzyjoin::fuzzy_left_join(
    x = df,
    y = df_y,
    by = by,
    match_fun = match_fun
  )
  
  # Clean the output, fuzzyjoin keeps the joined variables and suffixes things,
  # which is not desirable
  if (clean) {
    
    df <- df %>% 
      select(-dplyr::ends_with(".y")) %>% 
      dplyr::rename_all(~stringr::str_remove(., ".x$"))
    
  }
  
  # Drop the between variables
  if (drop_between) {
    
    # Get index for between
    index_between <- purrr::map_chr(match_fun, format) %>% 
      stringr::str_which("<|>")
    
    # Get names
    names_between <- unname(by[index_between])
    
    # Drop
    df <- select(df, -!!names_between)
    
  }
  
  return(df)
  
}
