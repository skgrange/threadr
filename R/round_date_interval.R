#' Function to round dates to arbitrary time intervals.
#' 
#' \code{round_date_interval} is useful when a date, or a vector of dates, are
#' to be rounded to to intervals such as 5-seconds, 5-minutes, or 15-minutes.
#'
#' \code{round_date_interval} rounds dates to the nearest arbitrary time 
#' interval unlike \code{xts::align.time} which always pushes dates to the next 
#' interval. Dates that fall in the centre of the time interval are rounded down.
#' 
#' \code{round_date_interval} works by first calculating an origin. This origin
#' is the first instant of the date's hour. \code{round_date_interval} then 
#' extracts the seconds-past-the-origin and rounds this value to the interval's 
#' multiple (in seconds). The rounded value and the origin are then summed and 
#' returned.
#' 
#' \code{round_date_interval} does not make any adjustments for duplicated dates 
#' in the input or output.
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
#' @param interval The time-interval the date is to be rounded to. 
#' 
#' @author Stuart K. Grange
#' 
#' @seealso See \code{\link{time_pad}}, \code{\link{lubridate::round_date}}
#' 
#' @examples
#' 
#' \dontrun{
#' # A messy date vector
#' date.vector <- c("2015-04-09 06:25:23", "2015-04-09 06:41:27",
#'                  "2015-04-09 07:02:27", "2015-04-09 07:50:40")
#' 
#' # Parse dates
#' date.vector <- lubridate::ymd_hms(date.vector)
#' 
#' # Round dates to 15 minute intervals
#' round_date_interval(date.vector, "15 min")
#' "2015-04-09 06:30:00 UTC" "2015-04-09 06:45:00 UTC" "2015-04-09 07:00:00 UTC"
#' "2015-04-09 07:45:00 UTC"
#' 
#' # Round dates to 5 minute intervals
#' round_date_interval(date.vector, "5 mins")
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
#' 
round_date_interval <- function (date, interval = "5 min") {
  
  # Parse argument into numeric value which represents second-multiple
  interval <- switch(interval,
                     "1 sec" =, "1 second" = 1, 
                     "2 sec" =, "2 secs" =, "2 seconds" = 2, 
                     "5 sec" =, "5 secs" =, "5 seconds" = 5, 
                     "10 sec" =, "10 secs" =, "10 seconds" = 10,
                     "15 sec" =, "15 secs" =, "15 seconds" = 15,
                     "20 sec" =, "20 secs" =, "20 seconds" = 20,
                     "30 sec" =, "30 secs" =, "30 seconds" = 30,
                     "60 sec" =, "60 secs" =, "60 seconds" =, "1 min" = 60, 
                     "2 min" =, "2 mins" =, "2 minutes" = 2 * 60, 
                     "5 min" =, "5 mins" =, "5 minutes" = 5 * 60, 
                     "10 min" =, "10 mins" =, "10 minutes" = 10 * 60,
                     "15 min" =, "15 mins" =, "15 minutes" = 15 * 60,
                     "20 min" =, "20 mins" =, "20 minutes" = 20 * 60,
                     "30 min" =, "30 mins" =, "30 minutes" =, "half hour" = 30 * 60,
                     "60 min" =, "60 mins" =, "60 minutes" =, "hour" = 60 * 60)
  
  # If interval is different than switch, stop
  if (is.null(interval)) {
    stop("'interval' is not supported.")
  }
  
  # Floor round date to hour, 0 minutes and seconds, an origin
  date.floor <- lubridate::floor_date(date, "hour")
  
  # Pull out the pieces of the date
  # Minutes
  minutes <- lubridate::minute(date)
  minutes.in.seconds <- minutes * 60
  
  # Seconds
  seconds <- lubridate::second(date)
  
  # Add minutes and seconds of date
  seconds.sum <- minutes.in.seconds + seconds
  
  # Round to nearest interval multiple of the second-interval
  second.rounded <- interval * round(seconds.sum / interval)
  
  # Add rounded seconds to floor rounded date
  date <- date.floor + lubridate::seconds(second.rounded)
  
  # Return
  date
  
}
