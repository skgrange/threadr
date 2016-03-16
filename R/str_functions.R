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
#' \code{str_trim_length} trims strings to a certain length, for example 30 
#' characters. 
#' 
#' \code{str_sentence_case} capitalises the first letter in a string and makes
#' all other characters lowercase. This is different than \code{str_proper_case}. 
#' 
#' \code{str_underscore} converts CamelCase and period.separated strings to
#' lower-case underscore_separated strings. 
#' 
#' \code{str_trim_many_spaces} will remove excess spaces between words in a 
#' string. 
#' 
#' \code{str_chop} will chop a string into a vector of fixed-width lengths. 
#' 
#' \code{str_drop_xml_tags} removes XML tags from strings. 
#' 
#' \code{str_rm_brackets_and_contents} will erase brackets (\code{(} and \code{)}
#' or \code{[} and \code{]}) and characters within them. 
#' 
#' \code{str_extract_digits} will extract digits/numbers from a string and convert
#' to a numeric data class if desired. 
#' 
#' @author Stuart K. Grange
#'
#' @export
str_proper_case <- function (x) stringi::stri_trans_totitle(x)


#' @rdname str_proper_case
#' 
#' @export
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
  if (underscore) date <- stringr::str_replace_all(date, " |:|-|/", "_")
  
  # Return 
  date
  
}


#' @rdname str_proper_case
#' @export
str_rm_non_ascii <- function (x) stringr::str_replace_all(x, "[^\\x00-\\x7F]", "")


#' @rdname str_proper_case
#' 
#' @export
str_trim_length <- function (string, length) {
  
  # Vectorise the trimming function
  string <- lapply(string, function (x) trim(x, length))
  string <- unlist(string)
  string
  
}

# Function which does the string trimming
trim <- function (string, length) 
  ifelse(!is.na(length), strtrim(string, length), string)


#' @rdname str_proper_case
#' 
#' @export
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


#' @rdname str_proper_case
#' 
#' @export
str_underscore <- function (x) {
  
  x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x <- gsub(".", "_", x, fixed = TRUE)
  x <- gsub(":", "_", x, fixed = TRUE)
  x <- gsub("\\$", "_", x)
  x <- gsub("__", "_", x)
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  x <- tolower(x)
  x
  
}


#' @rdname str_proper_case
#' 
#' @export
str_trim_many_spaces <- function (x) stringr::str_replace_all(x, "\\s+", " ")



# http://stackoverflow.com/questions/2247045/chopping-a-string-into-a-vector-of-fixed-width-character-elements
#' @rdname str_proper_case
#' 
#' @export
str_chop <- function (string, n) {
  
  vector <- substring(string, seq(1, nchar(string), n), seq(n, nchar(string), n))
  vector
  
}


#' @rdname str_proper_case
#' 
#' @export
str_drop_xml_tags <- function (string) {
  
  string <- stringr::str_replace_all(string, "<.*?>", "")
  string <- stringr::str_trim(string)
  string
  
}


#' @rdname str_proper_case
#' 
#' @export
str_rm_brackets_and_contents <- function (x, type = "round") {
  
  if (type == "round") 
    x <- stringr::str_replace_all(x, "\\s*\\([^\\)]+\\)", "")
  
  if (type == "square")
    x <- stringr::str_replace(x, "\\[.+?\\]\\s*", "")
  
  # Return
  x
  
}
  


#' @rdname str_proper_case
#'
#' @export
str_extract_digits <- function (x, as.numeric = TRUE) {
  
  # Replace characters
  x <- stringr::str_replace_all(x, "[[:alpha:]]", "")
  
  # Numeric class
  if (as.numeric) x <- as.numeric(x)
  
  # Return
  x
  
}
