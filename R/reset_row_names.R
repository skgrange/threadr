#' Function to reset row names in a data frame. 
#' 
#' \code{reset_row_names} can be used with magrittr's pipe (\code{%>%}). 
#' 
#' @author Stuart K. Grange
#' 
#' @export
reset_row_names <- function(df) {
  
  # Do
  row.names(df) <- NULL
  
  # Return
  df
  
}
