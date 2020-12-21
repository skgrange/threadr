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
#'   "68:10", "78:12:12", "4:04.1", "4:04", NA
#' )
#' 
#' # Parse
#' parse_time(times)
#' 
#' # As seconds
#' as.numeric(parse_time(times))
#' 
#' @export
parse_time <- function(x) {
  
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
  x <- hms::parse_hms(x)
  
  return(x)
  
}


format_hms_string <- function(x) {
  if (!is.na(x) && stringr::str_count(x, ":") == 1L) x <- stringr::str_c("00:", x)
  return(x)
}
