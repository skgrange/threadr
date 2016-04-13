#' Function to wrap \code{readxl::read_excel} and then do some formating. 
#' 
#' @param file File name of \code{.xls} or \code{.xlsx} file. 
#' 
#' @param sheet Sheet within \code{file} to read. \code{sheet} can be a name or 
#' an integer which indicates position.
#' 
#' @param col_names Use the first row as the data frame names? 
#' 
#' @param col_types Data-types of columns. 
#' 
#' @param na Missing value indicator. Default is \code{""}. 
#' 
#' @param skip Number of rows to skip before reading tabular data.
#' 
#' @param clean Should the names of the returned data frame be cleaned? Default
#' is \code{TRUE} and will return lower-case and underscore seperated names. 
#' 
#' @param convert Should \code{type.convert} be run on the returned data frame? 
#' This can help to get correct data-types. 
#' 
#' @param quiet Should the the reader's \code{cat} output be discarded?
#' 
#' @author Stuart K. Grange
#' 
#' @export
excel_read <- function(file, sheet = 1, col_names = TRUE, col_types = NULL, 
                       na = "", skip = 0, clean = TRUE, convert = FALSE,
                       quiet = FALSE) {
  
  # Read sheet
  if (quiet) {
    
    df <- quiet(readxl::read_excel(file, sheet, col_names, col_types, na, skip))
    
  } else {
    
    df <- readxl::read_excel(file, sheet, col_names, col_types, na, skip)
    
  }
  
  # Standard data frame
  df <- base_df(df)
  
  # Do some name things
  if (clean) {
    
    names(df) <- stringr::str_to_lower(names(df))
    names(df) <- stringr::str_replace_all(names(df), "\\.| ", "_")
    
  }
  
  # Data types
  if (convert) 
    df[] <- lapply(df, function(x) type.convert(as.character(x), as.is = TRUE))
  
  # Return
  df
  
}


#' @export
excel_sheets <- function(file, quiet = FALSE) {
  
  # Read sheet
  if (quiet) {
    
    x <- quiet(readxl::excel_sheets(file))
    
  } else {
    
    x <- readxl::excel_sheets(file)
    
  }
  
  # Return
  x
  
}
