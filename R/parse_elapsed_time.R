#' Function to parse elapsed time into seconds. 
#' 
#' \code{parse_elapsed_time} parses strings such as \code{"01:24:05"} (hour-
#' minute-second) and \code{"54:11"} (minute-second). \code{parse_elapsed_time} 
#' is useful for sports.
#' 
#' @param time String containing elapsed time. 
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' 
#' # With hour
#' parse_elapsed_time("01:24:05")
#' 
#' # Without hour
#' parse_elapsed_time("54:11")
#' 
#' # Fractional seconds are preserved
#' parse_elapsed_time("01:12:11.889")
#' parse_elapsed_time("20:53.25")
#' 
#' }
#' 
#' @import stringr
#' 
#' @export
parse_elapsed_time <- function(time) {
  
  # Prepare string
  # Trim
  time <- str_trim(time)
  time <- str_replace(time, "^:|'|‘|`", "")
  
  # Split and store
  time_split <- str_split_fixed(time, pattern = "\\.", n = 2)
  fractional_seconds <- time_split[, 2]
  time <- time_split[, 1]
  
  # Pad time
  time <- ifelse(str_count(time) == 4, str_c("00:0", time), time)
  time <- ifelse(str_count(time) == 5, str_c("00:", time), time)
  
  # Add date and fractional seconds
  time <- str_c("1970-01-01 ", time, ".", fractional_seconds)

  # Parse date
  time <- lubridate::ymd_hms(time, tz = "UTC", truncated = 3)
  
  # To numeric
  seconds <- as.numeric(time)
  
  # Return
  seconds
  
}
