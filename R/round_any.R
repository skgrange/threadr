#' Round to multiple of any number.
#'
#' @param x numeric, date-time (POSIXct), or \strong{hms} vector to round.
#' 
#' @param accuracy number to round to; for POSIXct or \strong{hms} objects, a 
#' number of seconds. 
#' 
#' @param f rounding function: \code{\link{floor}}, \code{\link{ceiling}} or
#'  \code{\link{round}}
#'  
#' @keywords manip
#'
#' @examples
#' 
#' round_any(135, 10)
#' round_any(135, 100)
#' round_any(135, 25)
#' round_any(135, 10, floor)
#' round_any(135, 100, floor)
#' round_any(135, 25, floor)
#' round_any(135, 10, ceiling)
#' round_any(135, 100, ceiling)
#' round_any(135, 25, ceiling)
#'
#' # For POSIXct data-types
#' round_any(Sys.time() + 1:10, 5)
#' round_any(Sys.time() + 1:10, 5, floor)
#' round_any(Sys.time(), 3600)
#' 
#' # For hms data-types
#' round_any(hms::as_hms(603), 60)
#' 
#' @export
round_any <- function(x, accuracy, f = round) {
  UseMethod("round_any")
}

#' @export
round_any.numeric <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

#' @export
round_any.POSIXct <- function(x, accuracy, f = round) {
  tz <- format(x[1], "%Z")
  xr <- round_any(as.numeric(x), accuracy, f)
  as.POSIXct(xr, origin="1970-01-01 00:00.00 UTC", tz = tz)
}

#' @export
round_any.hms <- function(x, accuracy, f = round) {
  xr <- round_any(as.numeric(x), accuracy, f)
  hms::as_hms(xr)
}
