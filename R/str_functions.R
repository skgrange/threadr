#' Functions to preform string operations which do not currently exist in
#' \code{stringr}. 
#' 
#' \code{str_date} returns the system's idea of the date as a character string.
#' 
#' \code{str_rm_non_ascii} removes all non-ASCII characters from a string.
#' 
#' \code{str_trim_length} trims strings to a certain length of characters. 
#' 
#' \code{str_to_sentence} capitalises the first letter in a string and makes
#' all other characters lowercase.
#' 
#' \code{str_to_underscore} converts CamelCase and period.separated strings to
#' lowercase underscore_separated strings. 
#' 
#' \code{str_trim_many_spaces} will remove excess spaces between words in a 
#' string. 
#' 
#' \code{str_chop} will chop a string into a vector of fixed-width lengths. 
#' 
#' \code{str_drop_xml_tags} removes XML tags from strings. 
#' 
#' \code{str_rm_brackets_and_contents} will erase brackets (\code{(} and \code{)}
#' or \code{[} and \code{]}) and the characters within them. 
#' 
#' \code{str_extract_digits} will extract digits/numbers from a string and convert
#' to a numeric data class if desired. 
#' 
#' \code{str_sql_quote} will add single quotes can collapse string vectors, a 
#' common step for building SQL statements. 
#' 
#' \code{str_unique} will find unique characters in a string. 
#' 
#' \code{str_nth_character} will return single character(s) from position(s) 
#' for a string. 
#' 
#' \code{str_create_na} will convert \code{"NA"}, \code{""}, and \code{" "} into
#' \code{NA}. 
#' 
#' \code{str_thousands_separator} will add thousands separators to a string or 
#' numeric vector. 
#' 
#' \code{str_note_to_title} will format a normalised note such as 
#' \code{"meeting_in_auckland"} to a note with title-case. 
#' 
#' \code{str_note_to_sentence} will format a normalised note such as 
#' \code{"testing_the_things"} to a note with the first character capitalised
#' with all other characters lower-case. 
#' 
#' \code{str_to_general} will make a "general" string and will transliterate 
#' special characters to avoid issues such as matching for a \code{join}. 
#' 
#' \code{str_filter} will filter a character vector to match a pattern, or drop
#' elements which match a pattern. 
#' 
#' @author Stuart K. Grange
#'
#' @export
str_date <- function(time = TRUE, tz = TRUE, underscore = FALSE) {
  
  if (!time) {
  
    date <- as.character(Sys.Date())  
    
  } else {
    
    date <- Sys.time()
    
    if (tz) {
      
      date <- format(date, usetz = TRUE)
      
    } else {
      
      date <- format(date, usetz = FALSE)
      
    }
    
  }
  
  # Useful for file names
  if (underscore) date <- stringr::str_replace_all(date, " |:|-|/", "_")
  
  # Return
  date
  
}


#' @rdname str_date
#' @export
str_rm_non_ascii <- function(x) stringr::str_replace_all(x, "[^\\x00-\\x7F]", "")


#' @rdname str_date
#' @export
str_trim_length <- function(string, length) {
  
  # Vectorise the trimming function
  string <- lapply(string, function(x) str_trim_length_worker(x, length))
  string <- unlist(string)
  string
  
}

# Function which does the string trimming
str_trim_length_worker <- function(string, length) 
  ifelse(!is.na(length), strtrim(string, length), string)


#' @rdname str_date
#' 
#' @export
str_to_sentence <- function(x) {
  
  # Get first character
  first <- stringr::str_sub(x, 1, 1)
  
  # Capitialise
  first <- stringr::str_to_upper(first)
  
  # Get other characters
  other_characters <- stringr::str_sub(x, 2)
  
  # Lower case
  other_characters <- stringr::str_to_lower(other_characters)
  
  # Combine again
  x <- stringr::str_c(first, other_characters)
  
  # Return
  x
  
}


#' @rdname str_date
#' 
#' @export
str_to_underscore <- function(x) {
  
  x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x <- gsub(".", "_", x, fixed = TRUE)
  x <- gsub(":", "_", x, fixed = TRUE)
  x <- gsub("\\$", "_", x)
  x <- gsub(" ", "_", x)
  x <- gsub("__", "_", x)
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  x <- tolower(x)
  x <- stringr::str_trim(x)
  x
  
}


#' @rdname str_date
#' 
#' @export
str_trim_many_spaces <- function(x) stringr::str_replace_all(x, "\\s+", " ")


# http://stackoverflow.com/questions/2247045/chopping-a-string-into-a-vector-of-fixed-width-character-elements
#' @rdname str_date
#' 
#' @export
str_chop <- function(string, n)
  substring(string, seq(1, nchar(string), n), seq(n, nchar(string), n))


#' @rdname str_date
#' 
#' @export
str_drop_xml_tags <- function(string) {
  
  string <- stringr::str_replace_all(string, "<.*?>", "")
  string <- stringr::str_trim(string)
  string
  
}


#' @rdname str_date
#' 
#' @export
str_rm_round_brackets <- function(x) 
  stringr::str_replace_all(x, "\\s*\\([^\\)]+\\)", "")


#' @rdname str_date
#' 
#' @export
str_rm_square_brackets <- function(x)
  stringr::str_replace_all(x, "\\[[^\\]]*\\]", "")
  

#' @rdname str_date
#'
#' @export
str_extract_digits <- function(x, as.numeric = TRUE) {
  
  # Replace characters
  x <- stringr::str_replace_all(x, "[[:alpha:]]", "")
  
  # Numeric class
  if (as.numeric) x <- as.numeric(x)
  
  # Return
  x
  
}


#' @rdname str_date
#'
#' @export
str_sql_quote <- function(x, collapse = ",") {
  
  # Quote strings
  x <- stringr::str_c("'", x, "'")
  
  # Make single element
  if (!is.na(collapse)) x <- stringr::str_c(x, collapse = collapse)
  
  # Return
  x
  
}


#' @rdname str_date
#'
#' @export
str_unique <- function(x) unique(strsplit(x, "")[[1]])


#' @rdname str_date
#'
#' @export
str_nth_character <- function(x, n) stringr::str_sub(x, start = n, end = n)


#' @rdname str_date
#'
#' @export
str_create_na <- function(x) ifelse(x %in% c("NA", "", " "), NA, x)


#' @rdname str_date
#'
#' @export
str_thousands_separator <- function(x, sep = " ")
  format(as.numeric(x), big.mark = sep, scientific = FALSE)


#' @rdname str_date
#'
#' @export
str_note_to_title <- function(x, sep = "_") {
  
  x <- stringr::str_to_lower(x)
  x <- stringr::str_replace_all(x, sep, " ")
  x <- stringr::str_to_title(x)
  x
  
}


#' @rdname str_date
#'
#' @export
str_note_to_sentence <- function(x, sep = "_") {
  
  x <- stringr::str_to_lower(x)
  x <- stringr::str_replace_all(x, sep, " ")
  x <- str_to_sentence(x)
  x
  
}


#' @rdname str_date
#'
#' @export
str_to_general <- function(x) stringi::stri_trans_general(x, "Latin-ASCII")


#' @rdname str_date
#'
#' @export
str_filter <- function(x, pattern, ignore.case = FALSE, invert = FALSE)
  grep(pattern, x, value = TRUE, ignore.case = ignore.case, invert = invert)
