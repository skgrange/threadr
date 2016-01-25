#' Function to round dates to arbitrary time intervals.
#' 
#' \code{round_date_interval} can be useful when a date, or a vector of dates, 
#' are to be rounded to intervals such as 5-seconds, 5-minutes, or 15-minutes.
#'
#' \code{round_date_interval} rounds dates to the nearest arbitrary time 
#' interval unlike \code{xts::align.time} which always pushes dates to the next 
#' interval. Dates that fall in the centre of the time interval are rounded down.
#' 
#' \code{round_date_interval} works by first calculating an origin. This origin
#' is the first instant of the date's hour. \code{round_date_interval} then 
#' extracts the seconds-past-the-origin and rounds this value to the interval's 
#' multiple. The rounded value and the origin are then summed and returned.
#' 
#' \code{round_date_interval} can be also used for rounding dates to the nearest
#' whole second, minute, or hour. However, \code{lubridate::round_date} and 
#' related functions may be more convenient. 
#' 
#' \code{round_date_interval} cannot deal with multiples other than seconds or 
#' minutes. Intervals such as 2 hours will not work.
#' 
#' The \code{interval} argument must include a number and a unit (for example: 
#' \code{15 seconds} or \code{5 min}). This has been done so that there is no 
#' ambiguity about the units which the date is to be rounded to. 
#' 
#' @param date A vector of dates to be rounded.
#' 
#' @param interval The time-interval the date is to be rounded to. 
#' 
#' @param f Rounding function to use: \code{\link{round}}, \code{\link{floor}}, 
#' or \code{\link{ceiling}}. Note that \code{f} is not quoted. 
#' 
#' @author Stuart K. Grange
#' 
#' @seealso See \code{\link{time_pad}}, \code{\link{round_date}}, 
#' \code{\link{round_any}}
#' 
#' @examples
#' \dontrun{
#' # A messy date vector
#' date_vector <- c("2015-04-09 06:25:23", "2015-04-09 06:41:27",
#'                  "2015-04-09 07:02:27", "2015-04-09 07:50:40")
#' 
#' # Parse dates
#' date_vector <- lubridate::ymd_hms(date_vector)
#' 
#' # Round dates to 15 minute intervals
#' round_date_interval(date_vector, "15 min")
#' "2015-04-09 06:30:00 UTC" "2015-04-09 06:45:00 UTC" "2015-04-09 07:00:00 UTC"
#' "2015-04-09 07:45:00 UTC"
#' 
#' # Round dates to 5 minute intervals
#' round_date_interval(date_vector, "5 mins")
#' "2015-04-09 06:25:00 UTC" "2015-04-09 06:40:00 UTC" "2015-04-09 07:00:00 UTC"
#' "2015-04-09 07:50:00 UTC"
#' 
#' 
#' # Round a single date to various second-intervals
#' # Parse
#' date <- lubridate::ymd_hms("2013-09-13 08:03:37")
#' 
#' # 10 seconds
#' round_date_interval(date, "10 sec")
#' "2013-09-13 08:03:40 UTC"
#' 
#' # 30 seconds
#' round_date_interval(date, "30 sec")
#' "2013-09-13 08:03:30 UTC"
#' 
#' }
#' 
#' @export
round_date_interval <- function (date, interval = "5 min", f = round) {
  
  # Parse argument into numeric value which represents second-multiple
  # Character catching
  interval <- stringr::str_replace_all(interval, "-|_|\\.", " ")
  
  interval <- switch(interval,
                     "1 sec"  =, "1 second" =, "sec" = 1, 
                     "2 sec"  =, "2 secs" =, "2 seconds" = 2, 
                     "5 sec"  =, "5 secs" =, "5 seconds" = 5, 
                     "10 sec" =, "10 secs" =, "10 seconds" = 10,
                     "15 sec" =, "15 secs" =, "15 seconds" = 15,
                     "20 sec" =, "20 secs" =, "20 seconds" = 20,
                     "30 sec" =, "30 secs" =, "30 seconds" = 30,
                     "60 sec" =, "60 secs" =, "60 seconds" =, "1 min" =, "min" = 60, 
                     "2 min"  =, "2 mins" =, "2 minutes" = 2 * 60, 
                     "5 min"  =, "5 mins" =, "5 minutes" = 5 * 60, 
                     "10 min" =, "10 mins" =, "10 minutes" = 10 * 60,
                     "15 min" =, "15 mins" =, "15 minutes" = 15 * 60,
                     "20 min" =, "20 mins" =, "20 minutes" = 20 * 60,
                     "30 min" =, "30 mins" =, "30 minutes" =, "half hour" = 30 * 60,
                     "60 min" =, "60 mins" =, "60 minutes" =, "hour" = 60 * 60)
  
  # If interval is different than switch, stop
  if (is.null(interval)) stop("'interval' is not supported.")
  
  # Use plyr, 
  date <- plyr::round_any(date, accuracy = interval, f = f)
  
  # Return
  date
  
}
