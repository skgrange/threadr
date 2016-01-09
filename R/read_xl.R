#' Function to wrap \code{readxl::read_excel}. 
#' 
#' @export
read_xl <- function (file, sheet = 1, col_names = TRUE, col_types = NULL, 
                     na = "", skip = 0) {
  
  # Read sheet
  df <- readxl::read_excel(file, sheet, col_names, col_types, na, skip)
  
  # Standard data frame
  df <- base_df(df)
  
  # Do some name things
  names(df) <- stringr::str_to_lower(names(df))
  names(df) <- stringr::str_replace_all(names(df), "\\.| ", "_")
  
  # Return
  df
  
}
