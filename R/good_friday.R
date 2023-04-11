#' Functions to calculate the dates of shifting holidays. 
#' 
#' @param year Years to calculate dates for. 
#' 
#' @return \code{POSIXct} vector.
#' 
#' @examples
#' 
#' # Calculate some holiday dates
#' good_friday()
#' easter_sunday()
#' easter_monday()
#' 
#' @export
good_friday <- function(year = lubridate::year(lubridate::now())) {
  as.POSIXct(timeDate::GoodFriday(year = year), tz = "UTC")
}


#' @rdname good_friday
#' @export
easter_sunday <- function(year = lubridate::year(lubridate::now())) {
  as.POSIXct(timeDate::EasterSunday(year = year), tz = "UTC")
}


#' @rdname good_friday
#' @export
easter_monday <- function(year = lubridate::year(lubridate::now())) {
  as.POSIXct(timeDate::EasterMonday(year = year), tz = "UTC")
}
