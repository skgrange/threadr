#' Functions to preform string operations which do not currently exist in
#' \code{stringr}. 
#' 
#' \code{str_date} returns the system's idea of the date as a character string.
#' 
#' \code{str_rm_non_ascii} removes all non-ASCII characters from a string.
#' 
#' \code{str_trim_length} trims strings to a certain length of characters. 
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
#' \code{str_parse_html_codes} will parse HTML character codes into text. 
#' 
#' \code{str_utf8_to_integer} will map a character vector to the UTF-8 integer
#' code points. 
#' 
#' \code{str_integer_to_utf8} will map an integer vector to UTF-8 charcters. 
#' 
#' \code{str_insert} will insert characters in location within a string.
#' 
#' \code{str_extract_characters} will extract characters and drop digits. 
#' 
#' \code{str_english_currency_format} will format a number for currency 
#' printing. 
#' 
#' @param x,string Input string. 
#' 
#' @param time,tz,underscore,length,n,as.numeric,collapse,sep,pattern,ignore.case,invert,currency
#' Function specific options. 
#' 
#' @author Stuart K. Grange
#'
#' @export
str_date <- function(time = TRUE, tz = TRUE, underscore = FALSE) {
  
  # No longer used
  .Defunct(msg = "`str_date` is no longer available.")
  
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
  
  return(date)
  
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
  return(string)
  
}

# Function which does the string trimming
str_trim_length_worker <- function(string, length) {
  ifelse(!is.na(length), strtrim(string, length), string)
}


#' @rdname str_date
#' 
#' @export
str_to_underscore <- function(x) {
  
  x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x <- gsub(".", "_", x, fixed = TRUE)
  x <- gsub(":", "_", x, fixed = TRUE)
  x <- gsub("\\$", "_", x)
  x <- gsub(" |-", "_", x)
  x <- gsub("__", "_", x)
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  x <- stringr::str_to_lower(x)
  x <- stringr::str_trim(x)
  return(x)
  
}


#' @rdname str_date
#' 
#' @export
str_trim_many_spaces <- function(x) {
  
  # No longer used
  .Defunct(msg = "`str_trim_many_spaces` is no longer available, please use `stringr::str_squish`.")
  
}


# http://stackoverflow.com/questions/2247045/chopping-a-string-into-a-vector-of-fixed-width-character-elements
#' @rdname str_date
#' 
#' @export
str_chop <- function(string, n) {
  substring(string, seq(1, nchar(string), n), seq(n, nchar(string), n))
}


#' @rdname str_date
#' 
#' @export
str_drop_xml_tags <- function(string) {
  
  string <- stringr::str_replace_all(string, "<.*?>", "")
  string <- stringr::str_trim(string)
  return(string)
  
}


#' @rdname str_date
#' 
#' @export
str_rm_round_brackets <- function(x) {
  stringr::str_replace_all(x, "\\s*\\([^\\)]+\\)", "")
}


#' @rdname str_date
#' 
#' @export
str_rm_square_brackets <- function(x) {
  stringr::str_replace_all(x, "\\[[^\\]]*\\]", "")
}


#' @rdname str_date
#'
#' @export
str_extract_digits <- function(x, as.numeric = TRUE) {
  
  # Replace characters
  x <- stringr::str_remove_all(x, "[[:alpha:]]")
  
  # Remove special characters too
  x <- stringr::str_remove_all(x, "_")
  
  # Numeric class
  if (as.numeric) x <- as.numeric(x)
  
  return(x)
  
}


#' @rdname str_date
#'
#' @export
str_sql_quote <- function(x, collapse = ",") {
  
  # Quote strings
  x <- stringr::str_c("'", x, "'")
  
  # Make single element
  if (!is.na(collapse)) x <- stringr::str_c(x, collapse = collapse)
  
  return(x)
  
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
str_thousands_separator <- function(x, sep = " ") {
  format(as.numeric(x), big.mark = sep, scientific = FALSE)
}


#' @rdname str_date
#'
#' @export
str_note_to_title <- function(x, sep = "_") {
  
  x <- stringr::str_to_lower(x)
  x <- stringr::str_replace_all(x, sep, " ")
  x <- stringr::str_to_title(x)
  return(x)
  
}


#' @rdname str_date
#'
#' @export
str_note_to_sentence <- function(x, sep = "_") {
  
  x <- stringr::str_to_lower(x)
  x <- stringr::str_replace_all(x, sep, " ")
  x <- stringr::str_to_sentence(x)
  return(x)
  
}


#' @rdname str_date
#'
#' @export
str_to_general <- function(x) stringi::stri_trans_general(x, "Latin-ASCII")


#' @rdname str_date
#'
#' @export
str_filter <- function(x, pattern, ignore.case = FALSE, invert = FALSE) {
  # Soon to be dropped
  .Deprecated(msg = "`str_filter` is deprecated, please use `stringr::str_subset`.")
  grep(pattern, x, value = TRUE, ignore.case = ignore.case, invert = invert)
}


#' @rdname str_date
#'
#' @export
str_parse_html_codes <- function(x) {
  sapply(x, str_parse_html_codes_worker, USE.NAMES = FALSE)
}


str_parse_html_codes_worker <- function(x) {
  
  XML::xpathApply(
    XML::htmlParse(
      x, 
      asText = TRUE
    ), 
    "//body//text()", 
    XML::xmlValue)[[1]] 
  
}


#' @rdname str_date
#'
#' @export
str_utf8_to_integer <- function(x) {
  
  # Check
  if (!any(nchar(unique(x)) == 1, na.rm = TRUE)) {
    stop("All inputs must be a single character...", call. = FALSE)
  }
  
  # Do
  x <- purrr::map_int(x, utf8ToInt)
  return(x)
  
}


#' @rdname str_date
#'
#' @export
str_integer_to_utf8 <- function(x) {
  
  if (inherits(x, "numeric")) x <- as.integer(x)
  
  if (class(x) != "integer") {
    stop("Input must be an integer vector...", call. = FALSE)
  }
  
  # Do
  x <- purrr::map_chr(x, intToUtf8)
  return(x)
  
}


#' @rdname str_date
#'
#' @export
str_insert <- function(x, n, sep) {
  
  x_start <- stringr::str_sub(x, start = 1, end = n)
  x_end <- stringr::str_sub(x, start = n + 1, end = -1)
  x <- stringr::str_c(x_start, sep, x_end)
  return(x)
  
}


#' @rdname str_date
#'
#' @export
str_extract_characters <- function(x) stringr::str_remove_all(x, "[[:digit:]]+")


#' @rdname str_date
#'
#' @export
str_english_currency_format <- function(x, sep = " ", currency = NA) {
  
  x <- x %>% 
    round(digits = 2) %>% 
    format(nsmall = 2, big.mark = sep, scientific = FALSE) %>% 
    stringr::str_trim()
  
  if (!is.na(currency[1])) x <- stringr::str_c(currency, " ", x)
  
  return(x)
  
}


#' @rdname str_date
#'
#' @export
str_url_decode <- function(x) purrr::map_chr(x, URLdecode)
