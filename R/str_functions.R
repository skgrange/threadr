#' Functions to preform string operations which do not currently exist in
#' \code{stringr}. 
#' 
#' \code{str_proper_case} capitalises the first letter of every word in a string. 
#' \code{str_proper_case} is a wrapper for \code{stringi::stri_trans_totitle}. 
#' 
#' \code{str_rm_non_ascii} removes all non-ASCII characters from a string.
#' 
#' \code{str_date} returns the system's idea of the date as a character string. 
#' 
#' \code{str_trim_length} trims strings to certain character lengths. 
#' 
#' \code{str_sentence_case} capitalises the first letter in a string and makes
#' all other characters lowercase. 
#' 
#' @author Stuart K. Grange
#'
#' @examples
#' 
#' \dontrun{
#' # Some strings
#' string <- c("auckland", "wellington", "berlin")
#' 
#' # Make proper case
#' str_proper_case(string)
#' "Auckland"   "Wellington" "Berlin"
#'}
#' 
#' @export
#'
str_proper_case <- function (x) stringi::stri_trans_totitle(x)


#' @rdname str_proper_case
#' 
#' @export
#' 
str_date <- function (time = TRUE, tz = TRUE, underscore = FALSE) {
  
  # tz argument is redundant if time is set to FALSE
  tz <- ifelse(!time, FALSE, tz)
  
  # If timezone information is desired, then time is needed too
  if (time | tz) {
    
    # Get date with time
    date <- as.character(Sys.time())
    
  } else {
    
    # Just the date
    date <- as.character(Sys.Date())
    
  }
  
  if (tz) {
    
    # Get time zone
    # To-do: do a unix work-around, returning NA if zone is not set
    time_zone <- Sys.timezone(location = TRUE)
    
    # Add time zone to string
    if (!is.na(time_zone)) {
      date <- paste(date, time_zone)
    }
    
  }
  
  # Useful for file names
  if (underscore) {
    date <- stringr::str_replace_all(date, " |:|-|/", "_")
  }
  
  # Return 
  date
  
}


#' @rdname str_proper_case
#'
#' @export
#' 
str_rm_non_ascii <- function (x) {
  
  # Remove non-ASCII characters
  x <- stringr::str_replace_all(x, "[^\\x00-\\x7F]", "")
  x
  
}


#' @rdname str_proper_case
#' 
#' @export
#' 
str_trim_length <- function (string, length) {
  string <- ifelse(!is.na(length), strtrim(string, length), string)
  string
}


#' @rdname str_proper_case
#' 
#' @export
#' 
str_sentence_case <- function (x) {
  
  # Get first character
  first <- substring(x, 1, 1)
  # Capitialise
  first <- toupper(first)
  
  # Get other characters
  other_characters <- substring(x, 1 + 1)
  # Lower case
  other_characters <- tolower(other_characters)
  
  # Combine again
  x <- stringr::str_c(first, other_characters)
  
  # Return
  x
  
}


 
# str_upper_first <- function (x, all = FALSE) {
#   
#   x <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", x, perl = TRUE)
#   x
#   
# }

