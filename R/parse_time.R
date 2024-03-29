#' Function to parse times into the \code{hms} data type. 
#' 
#' @param x Vector of time to be parsed. 
#' 
#' @author Stuart K. Grange
#' 
#' @return \code{hms} vector. 
#'  
#' @examples 
#' 
#' # Some different time formats
#' times <- c(
#'   "44:41", "44:41.3", "01:38:44.566", "1:38:4", "24:00:00", "24:00", 
#'   "68:10", "78:12:12", "4:04.1", "4:04", NA, "110:42:00"
#' )
#' 
#' # Parse
#' parse_time(times)
#' 
#' # As seconds
#' as.numeric(parse_time(times))
#' 
#' # parse_time will also handle numeric vectors, there is an assumption that
#' # the elements are stored as seconds
#' parse_time(c(55, 555.9, 60))
#' 
#' @export
parse_time <- function(x) {
  
  # If numeric, assume the vector is seconds, otherwise do some string processing
  if (is.numeric(x)) {
    x <- hms::as_hms(x)
  } else {
    
    # Format input
    x <- purrr::map_chr(x, format_hms_string)
    
    # Check input and raise warning
    if (any(stringr::str_count(x, "\\.") >= 2, na.rm = TRUE)) {
      warning(
        "Two periods (`.`) have been detected in input, this is probably an error...", 
        call. = FALSE
      )
    }
    
    # Parse string
    y <- hms::parse_hms(x)
    
    # If there are missing elements, try applying logic than handles >= 24 hours
    x <- if_else(is.na(y), parse_larger_than_24_hour_string(x), y)
    
  }
  
  return(x)
  
}


format_hms_string <- function(x) {
  if (!is.na(x) && stringr::str_count(x, ":") == 1L) x <- stringr::str_c("00:", x)
  return(x)
}


parse_larger_than_24_hour_string <- function(x) {
  
  x %>% 
    lubridate::hms(quiet = TRUE) %>% 
    as.numeric() %>% 
    hms::as_hms()
  
}
