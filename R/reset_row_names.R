#' Function to reset row names in a data frame. 
#' 
#' \code{reset_row_names} can be used with magrittr's pipe (\code{\%>\%}). 
#' 
#' @param df Data frame. 
#' 
#' @return Data frame. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
reset_row_names <- function(df) {
  
  row.names(df) <- NULL
  return(df)
  
}
