#' Function to drop columns in a data frame which only contain \code{NA}. 
#' 
#' @seealso \href{http://stackoverflow.com/questions/2643939/remove-columns-from-dataframe-where-all-values-are-na}{stackoverflow.com}
#'
#' @param df Data frame with \code{NA} columns.
#'
#' @author teucer
#' 
#' @return Data frame. 
#' 
#' @export
drop_na_columns <- function(df) {
  
  # Test variables for missing-ness
  index <- colSums(is.na(df)) < nrow(df)
  
  # Drop
  df <- df[, index, drop = FALSE]
  
  return(df)
  
}
