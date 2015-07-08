#' Function to drop columns in a data frame which only contain NA
#' 
#' See: http://stackoverflow.com/questions/2643939/remove-columns-from-dataframe-where-all-values-are-na
#'
#' @author teucer
#' 
#' @export
#' 
drop_na_columns <- function (df) {
    
  # Select with superscripts
  df <- df[, colSums(is.na(df)) < nrow(df)]
  
  # Return
  df
  
}
