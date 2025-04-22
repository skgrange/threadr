#' Function to return duplicated rows in a data frame. 
#' 
#' \code{duplicated_rows} can be though as the inverse of 
#' \code{\link[dplyr]{distinct}}. 
#' 
#' @param df Data frame. 
#' 
#' @param variable A vector of variables to test for uniqueness.
#' 
#' @param invert Should the function exclude duplicated. This is the same as
#' \code{\link[dplyr]{distinct}}. 
#' 
#' @return Data frame. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
duplicated_rows <- function(df, variable, invert = FALSE) {
  
  if (invert) {
    df <- df[!duplicated(df[, variable]), ]
  } else {
    df <- df[duplicated(df[, variable]), ]
  }
  
  return(df)
  
}
