#' Function to drop columns in a data frame which only contain NA
#' 
#' See: http://stackoverflow.com/questions/2643939/remove-columns-from-
#' dataframe-where-all-values-are-na
#'
#' @author teucer
#' 
#' 
#' @export
#' 
#' 
removeNACols <- function(x) { 
  
  x <- x[, colSums(is.na(x)) < nrow(x)]
  
  x
  
}