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
#' @param excel_bom Should a byte-order mark (BOM) be added to the file to 
#' indicate to Microsoft Excel that the file is \code{UTF-8} encoded. 
#' 
#' @return Invisible \code{df}.
#' 
#' @export
write_csv_threadr <- function(df, file, format_dates = TRUE, na = "",
                              excel_bom = FALSE) {
  
  # Check object
  stopifnot(inherits(df, "data.frame"))
  
  # Format dates
  if (format_dates) {
    df <- mutate(
      df, 
      across(
        tidyselect::vars_select_helpers$where(lubridate::is.POSIXt), 
        ~format_dates_for_export(.)
      )
    )
  }
  
  # Format other things
  df <- mutate(
    df, 
    across(tidyselect::vars_select_helpers$where(is.logical), as.integer)
  )
  
  # Export
  if (excel_bom) {
    readr::write_excel_csv(df, file = file, na = na)
  } else {
    readr::write_csv(df, file = file, na = na)
  }
  
  return(invisible(df))
  
}


format_dates_for_export <- function(date) {
  
  # Test for sub-second resolution
  # Get unique values
  date_unique <- unique(date)
  
  # Test for sub-seconds
  is_floor <- identical(
    date_unique, lubridate::floor_date(date_unique, "seconds")
  )
  
  # To a logical
  has_sub_seconds <- any(!is_floor)
  
  # The format string
  format_string <- if_else(
    has_sub_seconds, "%Y-%m-%d %H:%M:%S.%OS", "%Y-%m-%d %H:%M:%S"
  )
  
  date_string <- format(date, format = format_string)
  
  return(date_string)
  
}
