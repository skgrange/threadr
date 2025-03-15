#' Function to extract time-zone from a POSIXct date.  
#' 
#' @author Stuart K. Grange
#' 
#' @param date Date vector.
#' 
#' @return Vector with a length of one. 
#' 
#' @examples 
#' \dontrun{
#' 
#' # A date vector
#' time_zone(date_parsed)
#' 
#' # A variable in data frame
#' time_zone(data_test$date)
#' 
#' }
#' 
#' @export
time_zone <- function(date) attr(date, "tzone")


#' Function to conveniently parse a vector of Unix time to a POSIXct date vector. 
#' 
#' @author Stuart K. Grange
#' 
#' @param x An integer- or numeric-vector of Unix times. 
#' 
#' @param tz Time-zone string. \code{parse_unix_time} defaults to \code{"UTC"}. 
#' 
#' @param origin Origin of epoch. By definition, Unix time is \code{"1970-01-01"},
#' but other epochs can be used.
#' 
#' @return A POSIXct vector with the length of \code{x}.
#' 
#' @examples
#' 
#' # A vector
#' unix_time_vector <- c(1460034000, 1460034703)
#' 
#' # Parse time, will be in UTC time-zone
#' parse_unix_time(unix_time_vector)
#' 
#' # Or in Berlin's time-zone
#' parse_unix_time(unix_time_vector, tz = "Europe/Berlin")
#' 
#' @export 
parse_unix_time <- function(x, tz = "UTC", origin = "1970-01-01") {
  as.POSIXct(x, tz = tz, origin = origin)
}


#' Function to parse Microsoft Excel's numeric date. 
#' 
#' Depending on what version of Microsoft Excel, there are two origins used. 
#' 
#' @author Stuart K. Grange
#' 
#' @param x Numeric vector. 
#' 
#' @param tz Time-zone. Default is \code{"UTC"}. 
#' 
#' @param type Type of Microsoft Excel date. Can be \code{"windows"} or 
#' \code{"os_x_2007"}. 
#' 
#' @seealso \href{http://stackoverflow.com/questions/1703505/excel-date-to-unix-timestamp}{stackoverflow},
#' \code{\link{unix_time_to_excel_date}}
#' 
#' @return \code{POSIXct} vector.
#' 
#' @export
parse_excel_date <- function(x, tz = "UTC", type = "windows") {
  
  # Check
  type <- stringr::str_to_lower(type)
  type <- stringr::str_replace_all(type, "\\.| ", "_")
  
  if (!type %in% c("windows", "os_x_2007")) {
    stop("Type must be 'windows' or 'os_x_2007'", call. = FALSE)
  }
  
  # To numeric
  if (!inherits(x, "numeric")) x <- as.numeric(x)
  
  # To unix time, different origins depending on version
  if (type == "windows") x <- (x - 25569) * 86400
  if (type == "os_x_2007") x <- (x - 24107) * 86400
  
  # To POSIXct
  x <- parse_unix_time(x, tz = tz)
  
  return(x)
  
}


#' Function to convert unix time to a Microsoft Excel date. 
#' 
#' Depending on what version of Microsoft Excel, there are two origins used. 
#' 
#' @author Stuart K. Grange
#' 
#' @param x Numeric vector. 
#' 
#' @param tz Time-zone. Default is \code{"UTC"}. 
#' 
#' @param type Type of Microsoft Excel date. Can be \code{"windows"} or 
#' \code{"os_x_2007"}. 
#' 
#' @seealso \href{http://stackoverflow.com/questions/1703505/excel-date-to-unix-timestamp}{stackoverflow},
#' \code{\link{parse_excel_date}}
#' 
#' @return \code{POSIXct} vector.
#' 
#' @export
unix_time_to_excel_date <- function(x, tz = "UTC", type = "windows") {
  
  # Check
  type <- stringr::str_to_lower(type)
  type <- stringr::str_replace_all(type, "\\.| ", "_")
  
  if (!type %in% c("windows", "os_x_2007")) {
    stop("Type must be 'windows' or 'os_x_2007'", call. = FALSE)
  }
  
  # To numeric, why is this giving warnings? To-do figure out why.
  suppressWarnings(
    if (!inherits(x, "numeric")) x <- as.numeric(x)
  )
  
  # To Excel date, different origins depending on version
  if (type == "windows") x <- (x / 86400) + 25569
  if (type == "os_x_2007") x <- (x / 86400) + 24107
  
  return(x)
  
}


#' Function to test if dates are during the weekend or not. 
#' 
#' @author Stuart K. Grange
#' 
#' @param x Date vector.
#' 
#' @return Logical vector.
#' 
#' @export
weekend <- function(x) {
  
  x <- lubridate::wday(x)
  x <- if_else(x %in% c(1, 7), TRUE, FALSE)
  return(x)
  
}


#' Function to get weekday number from a date where \code{1} is Monday and 
#' \code{7} is Sunday. 
#' 
#' @author Stuart K. Grange
#' 
#' @param x Date vector.
#' 
#' @param as.factor Should the return be an (ordered) factor with weekday labels? 
#' 
#' @param abbr If \code{as.factor}, should the weekday lables be abbreviated?
#' 
#' @return Numeric or factor vector.
#' 
#' @export
wday_monday <- function(x, as.factor = FALSE, abbr = FALSE) {
  
  x <- lubridate::wday(x)
  x <- x - 1
  x <- ifelse(x == 0, 7, x)
  
  if (as.factor) {
    if (abbr) {
      labels <- c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")
    } else {
      labels <- c(
        "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", 
        "Sunday"
      )
    }
    
    # Give order and a label
    x <- factor(x, levels = 1:7, labels = labels)
    
  }
  
  return(x)
  
}


#' Functions to conveniently access number of seconds in different time periods. 
#' 
#' @param leap_year Should leap year logic be used? 
#' 
#' @param x A date or year vector. 
#' 
#' @return Integer vector with length of one. 
#' 
#' @author Stuart K. Grange
#' 
#' @rdname seconds_in_a_day
#' 
#' @export
seconds_in_a_day <- function() 86400L


#' @rdname seconds_in_a_day
#' @export
seconds_in_an_hour <- function() 3600L


#' @rdname seconds_in_a_day
#' @export
seconds_in_a_minute <- function() 60L


#' @rdname seconds_in_a_day
#' @export
hours_in_a_year <- function(leap_year = FALSE) if (!leap_year) 8760L else 8784L


#' @rdname seconds_in_a_day
#' @export
days_in_a_year <- function(leap_year = FALSE) if (!leap_year) 365L else 366L


#' @rdname seconds_in_a_day
#' @export
year_days <- function(x) {
  if_else(lubridate::leap_year(x), 366L, 365L)
}


#' @rdname seconds_in_a_day
#' @export
seconds_in_a_year <- function(leap_year = FALSE){
  seconds_in_a_day() * days_in_a_year(leap_year = leap_year)
}


#' @rdname seconds_in_a_day
#' @export
seconds_in_a_week <- function() seconds_in_a_day() * 7L


#' Function to determine season of a date.
#' 
#' The season coding is based on months of year. 
#' 
#' @param date \code{POSIXct} or \code{Date} vector. 
#' 
#' @param hemisphere Which hemisphere to use? Can be \code{"northern"} or 
#' \code{"southern"}.
#' 
#' @param as.factor Should the return be an ordered factor, not a numeric 
#' vector? 
#' 
#' @param as.title When \code{as.factor} is \code{TRUE}, should the labels be
#' in "title-case"?
#' 
#' @author Stuart K. Grange
#' 
#' @return Numeric or ordered factor vector with the length of \code{date}. 
#' 
#' @export
season <- function(date, hemisphere = "northern", as.factor = FALSE, 
                   as.title = FALSE) {
  
  # Check input
  hemisphere <- stringr::str_to_lower(hemisphere)
  
  if (!hemisphere %in% c("northern", "southern")) {
    stop("hemisphere must be 'northern' or 'southern'.")
  }
  
  # Get month of date
  x <- lubridate::month(date)
  
  if (hemisphere == "northern") {
    
    # Winter
    y <- if_else(x %in% c(12, 1:2), 1, 0)
    
    # Spring
    y <- if_else(x %in% 3:5, 2, y)
    
    # Summer
    y <- if_else(x %in% 6:8, 3, y)
    
    # Autumn
    y <- if_else(x %in% 9:11, 4, y)
    
    if (as.factor) {
      
      # Give order
      seasons_order <- c("winter", "spring", "summer", "autumn")
      if (as.title) seasons_order <- stringr::str_to_title(seasons_order)
      y <- ordered(y, levels = 1:4, labels = seasons_order)
      
    }
    
  } else {
    
    # Summer
    y <- if_else(x %in% c(12, 1:2), 1, 0)
    
    # Autumn
    y <- if_else(x %in% 3:5, 2, y)
    
    # Winter
    y <- if_else(x %in% 6:8, 3, y)
    
    # Spring
    y <-  if_else(x %in% 9:11, 4, y)
    
    if (as.factor) {
      
      # Give order
      seasons_order <- c("summer", "autumn", "winter", "spring")
      if (as.title) seasons_order <- stringr::str_to_title(seasons_order)
      y <- ordered(y, levels = 1:4, labels = seasons_order)
      
    }
    
  }
  
  return(y)
  
}


#' Function to return the system's idea of yesterday. 
#' 
#' @author Stuart K. Grange
#'
#' @param as_POSIXct Should the return be of \code{POSIXct} data type? 
#' 
#' @param tz If \code{as_POSIXct} is \code{TRUE}, what time zone should the 
#' return be in? 
#' 
#' @return \code{Date} or \code{POSIXct} vector with a length of 1. 
#' 
#' @examples 
#' 
#' # Date data type
#' yesterday()
#' 
#' # POSIXct data type
#' yesterday(as_POSIXct = TRUE)
#'
#' @export
yesterday <- function(as_POSIXct = FALSE, tz = "UTC") {
  
  x <- lubridate::today() - lubridate::days(1)
  if (as_POSIXct) x <- lubridate::ymd(x, tz = tz)
  return(x)
  
}


#' Function to get current system time at second accuracy. 
#' 
#' \code{now_to_the_second} floor rounds the current time. 
#' 
#' @author Stuart K. Grange
#' 
#' @param tz Time zone to represent dates in. The default is the system's time 
#' zone. 
#' 
#' @param as_numeric Should the time be returned as the numeric Unix time? 
#' 
#' @seealso \code{\link{now}}, \code{\link{Sys.time}}, \code{\link{floor_date}}
#' 
#' @return \code{POSIXct} or numeric vector with a length of 1.
#' 
#' @export
now_to_the_second <- function(tz = Sys.timezone(), as_numeric = FALSE) {
  
  # Get current time with sub-second accuracy
  x <- lubridate::now(tzone = tz)
  
  # Round date, floor rounding here
  x <- lubridate::floor_date(x, "second")
  
  if (as_numeric) {
    x <- as.numeric(x)
  }
  
  return(x)
  
}


#' Function to test a date (\code{}) vector for sub second accuracy.
#' 
#' @author Stuart K. Grange
#' 
#' @param date A date vector.
#' 
#' @return A logical vector with the length of 1. 
#' 
#' @export
has_sub_seconds <- function(date) {
  
  # Check data type
  stopifnot(inherits(date, "POSIXct"))
  
  # Check for missing elements
  stopifnot(!anyNA(date))
  
  # Get only unique values
  date_unique <- unique(date)
  
  # Test if input is identical to it's floor rounded representation 
  is_floor <- identical(
    date_unique, lubridate::floor_date(date_unique, "seconds")
  )
  
  # Do any elements fail the test?
  has_sub_seconds <- any(!is_floor)
  
  return(has_sub_seconds)
  
}
