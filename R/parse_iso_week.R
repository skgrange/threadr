#' Converts date from week notation according to ISO 8601 to standard notation
#'
#' This function returns the date of a given week-date (year, week of the year, 
#' day of week according to ISO 8601). 
#'
#' According to ISO 8601, the year of the week can differ from the calendar year.
#'
#' @param weekdate A character vector of year, week, and weekday in format 
#' "\code{\%Y-W\%V-\%u}"
#' 
#' @seealso \code{\link{strptime}} for a description of the date formats and 
#' references on ISO 8601.
#' 
#' @author Uwe Block \email{u.block.mz@@googlemail.com} (pulled from the ISOweek
#' package) with some edits by Stuart K. Grange. 
#' 
#' @return POSIXct date vector.
#' 
#' @examples
#' 
#' w <- paste("2009-W53", 1:7, sep = "-")
#' tibble::tibble(weekdate = w, date = parse_iso_week(w))
#' 
#' @export
parse_iso_week <- function(weekdate) {
  
  kPattern <- "^([0-9]{4})-W([0-9]{2})-([0-9]{1})$"
  # not used kPattern <- "^([0-9]{4})-W([0][1-9]|[1-4][0-9]|[5][0-3])-([1-7]{1})$"
  
  # instead check ranges separately
  stopifnot(all(is.na(weekdate) | stringr::str_detect(weekdate, kPattern)))
  
  wd_ywd <- stringr::str_match(weekdate, kPattern)
  
  # take care of all NA input because this will break the split into 4 columns
  if (all(is.na(weekdate))) {
    return(rep(as.Date(NA_character_), length.out = length(weekdate)))
  }
  
  stopifnot(ncol(wd_ywd) == 4)
  
  year <- wd_ywd[, 2]
  week <- as.integer(wd_ywd[, 3])
  weekday <- as.integer(wd_ywd[, 4])
  
  stopifnot(all(is.na(week) | (1 <= week & week <= 53)))
  stopifnot(all(is.na(weekday) | (1 <= weekday & weekday <= 7)))
  
  # first week of the year includes always the 4th of January,
  # take care of NA dates
  january04 <- as.Date(ifelse(is.na(year), NA, paste(year, "01", "04", sep="-")))
  # first thursday of the year
  first_thursday <- thursday0(january04)
  # advance by week-1 thursdays
  nearest_thursday <- first_thursday + 7 * (week - 1)
  
  # correct for weekday
  date <- nearest_thursday - 4 + weekday
  
  # Parse again to make a POSIXct date, not using as.POSIXct because of time-zone
  # issues
  date <- lubridate::ymd(date, tz = "UTC")
  
  return(date)
  
}


weekday0 <- function(date) {
  return(ISOweekday(date) - 1L)
}


thursday0 <- function(date) {
  date <- as.Date(date)
  return(date - weekday0(date) + 3)
}


year0 <- function(date) {
  date <- as.Date(date)
  return(as.integer(format(date, "%Y")))
}


ISOweekday <- function(date) {
  date <- as.Date(date)
  return(as.integer((as.integer(format(date, "%w"))+6) %% 7 + 1))
}
