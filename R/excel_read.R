#' Function to wrap \code{readxl::read_excel} and then do some header cleaning. 
#' 
#' @export
excel_read <- function (file, sheet = 1, col_names = TRUE, col_types = NULL, 
                        na = "", skip = 0, clean = TRUE, convert = FALSE) {
  
  # Read sheet
  df <- readxl::read_excel(file, sheet, col_names, col_types, na, skip)
  
  # Standard data frame
  df <- base_df(df)
  
  # Do some name things
  if (clean) {
    names(df) <- stringr::str_to_lower(names(df))
    names(df) <- stringr::str_replace_all(names(df), "\\.| ", "_")
  }
  
  # Data types
  if (convert) df[] <- lapply(df, function (x) type.convert(as.character(x)))
  
  # Return
  df
  
}


#' @export
excel_sheets <- function (file) readxl::excel_sheets(file)
