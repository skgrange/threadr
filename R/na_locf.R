#' Function for carrying last observation forwards. 
#' 
#' Used to replace each \code{NA} with the most recent non-\code{NA} prior to 
#' it. \code{locf} is a simple wrapper for \code{zoo::na.locf}.
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
#' data_income$income_chunk <- locf(data_income$income_chunk)
#' 
#' }
#' 
#' @export
na_locf <- function (x, na.rm = FALSE) zoo::na.locf(x, na.rm = na.rm)


#' Function for linearly interpolating \code{NA}s. 
#' 
#' @param x Input vector. 
#' 
#' @param na.rm Should leading NAs be removed?
#' 
#' @return Vector length of \code{x}. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
na_interpolate <- function(x, na.rm = FALSE) zoo::na.approx(x, na.rm = na.rm)
