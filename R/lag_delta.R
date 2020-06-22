#' Functions to calculate the delta between a value and the lagged or leading 
#' value. 
#' 
#' @author Stuart K. Grange. 
#' 
#' @param x Numeric vector. 
#' 
#' @return Numeric vector.
#' 
#' @examples 
#' 
#' lag_delta(sample(seq(0:10)))
#'
#' @export
lag_delta <- function(x) {
  x - dplyr::lag(x)
}


#' @rdname lag_delta
#' @export
lead_delta <- function(x) {
  x - dplyr::lead(x)
}
