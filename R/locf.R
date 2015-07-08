#' Function for carrying last observation forwards. 
#' 
#' Used to replace each NA with the most recent non-NA prior to it.
#' 
#' A wrapper for zoo::na.locf with na.rm defaulting to FALSE.
#' 
#' @author Stuart K. Grange
#' 
#' @export
#' 
locf <- function (x, na.rm = FALSE) {
  
  # Carry observation forwards
  x <- zoo::na.locf(x, na.rm = na.rm)
  
  # Return
  x
  
}
