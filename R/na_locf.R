#' Function for carrying last observation forwards. 
#' 
#' Used to replace each \code{NA} with the most recent non-\code{NA} prior to 
#' it. \code{locf} is a simple wrapper for \code{\link[zoo]{na.approx}}. 
#' 
#' @param x Input vector. 
#' 
#' @param na.rm Should leading NAs be removed? Default is \code{FALSE}. 
#' 
#' @return Vector of same data type and length of \code{x}. 
#' 
#' @author Stuart K. Grange
#' 
#' @examples
#' \dontrun{
#' 
#' # Push observations forwards, in a data frame
#' data_income$income_chunk <- na_locf(data_income$income_chunk)
#' 
#' }
#' 
#' @export
na_locf <- function(x, na.rm = FALSE) zoo::na.locf(x, na.rm = na.rm)


#' Function for interpolating \code{NA}s. 
#' 
#' \code{na_interpolate} wraps functions from the \strong{zoo} package. 
#' 
#' @param x Input vector. 
#' 
#' @param na.rm Should leading NAs be removed?
#' 
#' @param spline Should \code{NA}s be replaced using cubic splines rather than 
#' linear interpolation. 
#' 
#' @param extrapolate Should leading and preceding \code{NA}s be extrapolated 
#' too? Only used for linear interpolation.
#' 
#' @return Vector length of \code{x}. 
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \code{\link[zoo]{na.approx}}
#' 
#' @export
na_interpolate <- function(x, na.rm = FALSE, spline = FALSE, extrapolate = FALSE) {
  
  if (!spline) {
    x <- zoo::na.approx(x, na.rm = na.rm, rule = ifelse(extrapolate, 2, 1))
  } else {
    x <- zoo::na.spline(x, na.rm = na.rm)
  }
  
  return(x)
  
}
