#' Function to export data frame/tibble to a \code{.csv} text file with certain 
#' defaults. 
#' 
#' @author Stuart K. Grange
#' 
#' @param df A data frame or tibble to write to disk.
#' 
#' @param file File to write \code{df} to. 
#' 
#' @param format_dates Should the dates in \code{df} be converted to a 
#' \code{\%Y-\%m-\%d \%H:\%M:\%S} string? 
#' 
#' @param na String used for missing values. 
#' 
#' @return Invisible \code{df}.
#' 
#' @export
write_csv_threadr <- function(df, file, format_dates = TRUE, na = "") {
  
  # Check object
  stopifnot(inherits(df, "data.frame"))
  
  # Format dates
  if (format_dates) {
    df <- mutate(
      df, 
      across(
        tidyselect::vars_select_helpers$where(lubridate::is.POSIXt), 
        format,
        format = "%Y-%m-%d %H:%M:%S"
      )
    )
  }
  
  # Format other things
  df <- mutate(
    df, 
    across(tidyselect::vars_select_helpers$where(is.logical), as.integer)
  )
  
  # Export
  readr::write_csv(df, file = file, na = na)
  
  return(invisible(df))
  
}
