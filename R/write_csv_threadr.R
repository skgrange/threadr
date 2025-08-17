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
#' @param append If \code{TRUE}, data will be appended to an existing file. 
#' 
#' @param show_progress Should \strong{readr}'s progress messaging be displayed? 
#' 
#' @return Invisible \code{df}.
#' 
#' @export
write_csv_threadr <- function(df, file, format_dates = TRUE, na = "",
                              excel_bom = FALSE, append = FALSE, 
                              show_progress = FALSE) {
  
  # Check object
  stopifnot(inherits(df, "data.frame"))
  
  # Switch off progress
  if (!show_progress) {
    # Store setting
    progress_setting <- options()$readr.show_progress
    
    if (is.null(progress_setting)) {
      progress_setting <- TRUE
    }
    
    # Switch off progress
    options(readr.show_progress = show_progress)
  }
  
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
    readr::write_excel_csv(df, file = file, na = na, append = append)
  } else {
    readr::write_csv(df, file = file, na = na, append = append)
  }
  
  # Return progress setting
  if (!identical(progress_setting, show_progress)) {
    options(readr.show_progress = progress_setting)
  }
  
  return(invisible(df))
  
}


format_dates_for_export <- function(date) {
  
  # Test for sub-second resolution
  has_sub_seconds <- has_sub_seconds(date)
  
  # The format string
  format_string <- if_else(
    has_sub_seconds, "%Y-%m-%d %H:%M:%S.%OS", "%Y-%m-%d %H:%M:%S"
  )
  
  # Do the string processing
  date_string <- format(date, format = format_string)
  
  return(date_string)
  
}
