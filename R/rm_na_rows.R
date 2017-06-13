#' Function to remove rows in a data frame when indices are all \code{NA}. 
#' 
#' \code{rm_na_rows} is useful when multiple variables/columns want to be 
#' evaluated for \code{NA}s and if all variables are \code{NA}, remove the row.
#' \code{rm_na_rows} avoids the use of many \code{!is.na(variable)} filters for 
#' wide data frames.
#' 
#' @param df Data frame to be filtered.
#' 
#' @param index Column index of \code{df} to be evaluated for \code{NA} testing. 
#' \code{index} usually takes multiple values and handles negative indices. 
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' 
#' Remove rows which contain only NAs in all but the first two columns
#' data_clean <- rm_na_rows(data, -1:-2)
#' 
#' }
#' 
#' @export 
rm_na_rows <- function(df, index = NA) {
  
  # If no index is given, default to all rows
  if (is.na(index[1])) index <- 1:ncol(df)
  
  if (length(index) == 1) {
    
    # Simple filtering for single column
    df <- df[!is.na(df[, index]), ]
    
  } else {
    
    # The indices of rows with NAs
    indices <- apply(df[, index], 1, function(x) all(is.na(x)))
    
    # Filter the rows
    df <- df[!indices, ]
    
  }
  
  # Remove row names
  row.names(df) <- NULL
  
  # Return
  df
  
}
