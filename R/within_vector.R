#' Function to vectorise \code{lubridate::`\%within\%`}.
#' 
#' @author Stuart K. Grange
#' 
#' @param date Date vector. 
#' 
#' @param interval Interval vector. 
#' 
#' @return Logical vector with the length of \code{date}. 
#' 
#' @importFrom lubridate %within%
#' 
#' @examples 
#' \dontrun{
#' 
#' # Test dates within a data frame
#' data_temp$test <- interval_test(data_temp$date, interval)
#' 
#' }
#' 
#' @export
`%within_vector%` <- function (date, interval)
  sapply(date, function (x) any(ifelse(x %within% interval, TRUE, FALSE)))
