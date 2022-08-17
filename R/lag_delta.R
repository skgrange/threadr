#' Functions to calculate the delta between a value and the lagged or leading 
#' value. 
#' 
#' @author Stuart K. Grange. 
#' 
#' @param x Numeric vector. 
#' 
#' @param na_as_zero Should missing elements be set to 0? 
#' 
#' @return Numeric vector.
#' 
#' @examples 
#' 
#' lag_delta(sample(seq(0:10)))
#'
#' @export
lag_delta <- function(x, na_as_zero = FALSE) {
  x <- x - dplyr::lag(x)
  if (na_as_zero) x <- ifelse(is.na(x), 0, x)
  return(x)
}


#' @rdname lag_delta
#' @export
lead_delta <- function(x, na_as_zero = FALSE) {
  x <- x - dplyr::lead(x)
  if (na_as_zero) x <- ifelse(is.na(x), 0, x)
  return(x)
}
