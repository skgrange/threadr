#' Function to get Monday-based week number of a date. 
#' 
#' @param date Vector containing parsed dates. 
#' 
#' @param floor Should weeks which belong to the 53 rd week for the year be 
#' rounded down to the 52 nd week? Default is \code{FALSE}. 
#' 
#' @return Integer vector. 
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \code{\link[lubridate]{wday}}, \code{\link{week_financial}}
#' 
#' @export
week_monday <- function(date, floor = FALSE) {
  
  # If date, not POSIXct
  if (class(date)[1] == "Date") date <- lubridate::ymd(date, tz = "UTC")
  
  # Only days needed
  date <- lubridate::floor_date(date, "day")

  # To date frame
  df_days <- monday_week_table(date, floor)
  
  # Join
  df <- data.frame(date) %>% 
    left_join(df_days, "date")
  
  # Return vector
  as.integer(df$week_monday)
  
}


# Function to build look-up table
monday_week_table <- function(date, floor) {
  
  # Round to contain all years days
  date_start <- lubridate::floor_date(min(date, na.rm = TRUE), "year")
  date_end <- lubridate::ceiling_date(max(date, na.rm = TRUE), "year")
  
  # Create day sequence and add year
  df_days <- data.frame(date = seq(date_start, date_end, by = "days")) %>% 
    mutate(year = lubridate::year(date))
  
  # Calculate Monday weeks
  df_days <- df_days %>% 
    group_by(year) %>% 
    mutate(weekday = lubridate::wday(date), 
           monday_logical = ifelse(weekday == 2, 1, 0),
           week_monday = cumsum(monday_logical),
           week_monday = week_monday + 1) %>% 
    ungroup()
  
  if (floor) {
    
    # No 53 rd week
    df_days$week_monday <- ifelse(
      df_days$week_monday >= 52, 52, df_days$week_monday)
    
  }
  
  # Return
  df_days
  
}


#' Function to calculate financial week of the year. 
#' 
#' @param date Vector containing parsed dates. 
#' 
#' @param start The starting month of the financial year. Currently, the only
#' option is \code{"july"}. 
#' 
#' @param floor Should weeks which belong to the 53 rd week for the year be 
#' rounded down to the 52 nd week? Default is \code{TRUE}. 
#' 
#' @return Integer vector. 
#' 
#' @seealso \code{\link[lubridate]{wday}}, \code{\link{week_monday}}
#' 
#' @examples
#' \dontrun{
#' data_hours$week <- week_financial(data_hours$date)
#' }
#' 
#' @author Stuart K. Grange
#' 
#' @export
week_financial <- function(date, start = "july", floor = TRUE) {
  
  .Deprecated()
  
  # If date, not POSIXct
  if (class(date)[1] == "Date") date <- lubridate::ymd(date, tz = "UTC")
  
  # Only days needed
  date <- lubridate::floor_date(date, "day")
  
  # Create look-up table
  df_days <- financial_week_table(date, start, floor)
  
  # Join
  df <- data.frame(date) %>% 
    left_join(df_days, "date")
  
  # Return vector
  as.integer(df$week_financial)
  
}


# Function to build look-up table
financial_week_table <- function(date, start, floor) {
  
  # Lower case
  start_month <- stringr::str_to_lower(start)
  
  # Subtract a year so start of financial year is always present
  date_start <- min(date, na.rm = TRUE) - lubridate::years(1)
  
  if (start_month == "july")
    # Get start of financial year for date range, floor rounding
    date_start <- lubridate::ymd(stringr::str_c(lubridate::year(date_start), "-07-01"))
  
  # Get max date in vector
  date_end <- max(date, na.rm = TRUE)
  
  # Create day sequence and add financial year
  df_days <- data.frame(date = seq(date_start, date_end, "days")) %>% 
    mutate(year_financial = year_financial(date, start))
  
  # Calculate financial weeks
  # Start day is Saturday
  df_days <- df_days %>% 
    group_by(year_financial) %>% 
    mutate(weekday = lubridate::wday(date), 
           saturday_logical = ifelse(weekday == 7, 1, 0),
           week_financial = cumsum(saturday_logical),
		   week_financial = week_financial + 1) %>% 
    ungroup()
	
  if (floor) 
    # No 53 rd week
    df_days$week_financial <- ifelse(
	    df_days$week_financial >= 52, 52, df_days$week_financial)
  
  # Return
  df_days
  
}


#' Function to transform date into financial year. 
#' 
#' @param date Vector of parsed dates
#' 
#' @param start The starting month of the financial year. Currently, the only
#' option is \code{"july"}.
#' 
#' @author Stuart K. Grange 
#' 
#' @return Integer vector. 
#' 
#' @seealso \code{\link[lubridate]{year}}, \code{\link{week_financial}}
#' 
#' @examples
#' \dontrun{
#' 
#' year_financial(data_hours$date)
#' 
#' }
#' 
#' @export
year_financial <- function(date, start = "july") {
  
  .Deprecated()
  
  # Get year
  year <- lubridate::year(date)
  
  # Push year forwards
  if (stringr::str_to_lower(start) == "july")
    year <- ifelse(month(date) >= 7, year + 1, year)
  
  # Return
  as.integer(year)
  
}


#' Function to transform date into financial period (a month with an offset).  
#' 
#' @param date Vector of parsed dates
#' 
#' @param start The starting month of the financial year. Currently, the only
#' option is \code{"july"}. 
#' 
#' @author Stuart K. Grange 
#' 
#' @return Integer vector. 
#' 
#' @seealso \code{\link[lubridate]{wday}}, \code{\link{week_financial}}, 
#' \code{\link[lubridate]{month}}
#' 
#' @examples
#' \dontrun{
#' 
#' period_financial(data_hours$date)
#' 
#' }
#' 
#' @export
period_financial <- function(date, start = "july") {
  
  .Deprecated()
  
  # If date, not POSIXct
  if (class(date)[1] == "Date") date <- lubridate::ymd(date, tz = "UTC")
  
  # Transform date vector to data frame for joining
  df <- data.frame(date) %>% 
    mutate(month = lubridate::month(date))
  
  # Build look-up table
  if (start == "july"){
    
    # Build look up table
    df_look <- data.frame(
      month = seq(1, 12),
      period = c(seq(7, 12), seq(1, 6))
    )
    
  }
  
  # Join period
  df <- df %>% 
    left_join(df_look, "month")
  
  # Return vector
  as.integer(df$period)
  
}
