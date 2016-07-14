#' Function to parse elapsed time into seconds. 
#' 
#' \code{parse_elapsed_time} parses strings such as \code{"01:24:05"} (hour-
#' minute-second) and \code{"54:11"} (minute-second). Useful for sports. 
#' 
#' To-do: Parse strings without hour, but with fractional seconds. 
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
#' 
#' }
#' 
#' @import stringr
#' 
#' @export
parse_elapsed_time <- function(time) {
  
  # Pad time
  time <- str_trim(time)
  time <- str_replace(time, "^:", "")
  time <- ifelse(str_count(time) == 4, str_c("00:0", time), time)
  time <- ifelse(str_count(time) == 5, str_c("00:", time), time)
  
  # Add date
  time <- str_c("1970-01-01 ", time)
  
  # Parse date
  time <- lubridate::ymd_hms(time, tz = "UTC", truncated = 3)
  
  # To numeric
  seconds <- as.numeric(time)
  
  # Return
  seconds
  
}
