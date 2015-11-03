#' Function for carrying last observation forwards. 
#' 
#' Used to replace each \code{NA} with the most recent non-\code{NA} prior to it.
#' 
#' A simple wrapper for \code{zoo::na.locf} with \code{na.rm} defaulting to 
#' \code{FALSE}
#' 
#' @author Stuart K. Grange
#' 
#' @examples
#' \dontrun{
#' # Push observations forwards
#' data_income$income_chunk <- locf(data_income$income_chunk)
#' }
#' 
#' @export
locf <- function (x, na.rm = FALSE) {
  
  # Carry observation forwards
  x <- zoo::na.locf(x, na.rm = na.rm)
  
  # Return
  x
  
}
