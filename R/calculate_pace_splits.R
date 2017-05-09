#' Function to calculate splits for a running or cycling event. 
#' 
#' @param distance Target distance in kilometres. 
#' 
#' @param time Target time to cover \code{distance} as a string. Use a format 
#' like \code{"01:30:00"} for one hour and thirty minutes. 
#' 
#' @param interval Interval to calculate splits for in kilometres. Default is 
#' \code{1}. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Data frame. 
#' 
#' @examples 
#' \dontrun{
#'
#' # Pace for a half marathon
#' # Every km
#' calculate_pace_splits(half_marathon(), "01:30:00", 1)
#' 
#' # Every 5 km
#' calculate_pace_splits(half_marathon(), "01:30:00", 5)
#' 
#' 
#' # Pace for 10 km 
#' calculate_pace_splits(10, "30:00")
#' 
#' # Pace for 5 km 
#' calculate_pace_splits(5, "18:15")
#' 
#' }
#' 
#' 
#' @export
calculate_pace_splits <- function(distance, time, interval = 1, round = 2) {
  
  # http://www.coolrunning.com/engine/4/4_1/96.shtml
  
  # Pad string
  if (str_count(time, ":") == 1) time <- str_c("00:", time)
  
  # Time string to seconds
  time_seconds <- lubridate::hms(time)
  time_seconds <- lubridate::period_to_seconds(time_seconds)
  
  # Distance to metres
  distance_metres <- distance * 1000
  interval_metres <- interval * 1000
  
  # Calculate speed, ms -1
  speed_ms <- distance_metres / time_seconds
  speed_km_h <- ms_to_km_h(speed_ms)
  
  # Create distance sequence
  distance_sequence <- seq(interval_metres, distance_metres, by = interval_metres)
  distance_sequence_final <- tail(distance_sequence, 1)
  
  # Get extra element of distance
  distance_final <- floor(distance_sequence_final)
  
  # Append the vector if needed
  if (distance_sequence_final != distance_metres) {
    
    distance_extra <- distance_metres - distance_final
    distance_extra <- distance_final + distance_extra
    
    # Add to sequence
    distance_sequence <- c(distance_sequence, distance_extra)
    
  }
  
  # Calculate time
  time_elapsed <- distance_sequence / speed_ms
  
  # To date
  time_elapsed <- parse_unix_time(time_elapsed)
  
  # Format for printing/to character
  time_elapsed <- format(time_elapsed, format = "%H:%M:%OS")
  
  # Back to km
  distance_sequence <- distance_sequence / 1000
  
  # Add min km-1
  pace_min_km <- km_h_to_min_km(speed_km_h)
  pace_min_km <- decimal_minute_to_string(pace_min_km)
  
  # Create data frame
  df <- data.frame(
    distance,
    time, 
    speed_ms,
    speed_km_h,
    pace_min_km,
    distance_split = distance_sequence,
    time_split = time_elapsed,
    stringsAsFactors = FALSE
  )
  
  # Round
  df <- round_numeric(df, round = round)
  
  # Return, standard data frame
  df
  
}
