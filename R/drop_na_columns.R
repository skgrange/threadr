#' Function to drop columns in a data frame which only contain \code{NA}. 
#' 
#' @seealso \href{http://stackoverflow.com/questions/2643939/remove-columns-from-dataframe-where-all-values-are-na}{stackoverflow.com}
#'
#' @param df Data frame with \code{NA} columns.
#'
#' @author teucer
#' 
#' @export
drop_na_columns <- function(df) {
  
  # Test variables for missing-ness
  index <- colSums(is.na(df)) < nrow(df)
  
  # Drop
  df <- df[, index]
  
  # If subsetting has simplified object, make data frame again
  if (any(grepl("data.frame", class(df)))) {
    
    # Make data frame again
    df <- data.frame(
      df, 
      stringsAsFactors = FALSE
    )
    
    # Give names
    names(df) <- names(index[index])
    
  }
  
  # Return
  df
  
}
