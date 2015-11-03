#' Function to extract file metadata with \code{exiftool} and store it within a 
#' data frame. 
#' 
#' \code{file_metadata} uses Phil Harvey's Perl \code{exiftool} to extract a 
#' file's metadata and then formats the output into a table which can be 
#' conveniently used within R. 
#' 
#' \code{exiftool} is called as a system command and this function has not been
#' tested on other non-Debian systems. I do not know how \code{exiftool} is 
#' implemented on Windows or OS X and therefore I cannot guarantee that this 
#' function will work. 
#' 
#' \code{file_metadata} can be useful for media files and the exploration of 
#' metadata. 
#' 
#' All variables which are supported and returned by \code{exiftool} will be 
#' present. Watch out for very large numbers of variables when imputing large
#' XML or JSON documents with a large numbers of nested elements.
#' 
#' @param file The file to extract metadata from. 
#' 
#' @author Stuart K. Grange
#'
#' @examples
#' \dontrun{
#' data_metadata <- file_metadata("music.mp3")
#' }
#'
#' @export
file_metadata <- function (file, progress = "text") {
  
  # Vectorise function
  df <- plyr::ldply(file, scraper, .progress = progress)
  df
  
}


# The function which does the work
#
# No export 
#
scraper <- function (file) {

  # Get file basename
  file_basename <- basename(file)
  
  # Escape for bash
  file <- gsub("$", "\\$", file, fixed = TRUE)
  file <- gsub("`", "\\`", file, fixed = TRUE)

  # Build system command
  # Watch the different types of quotes here
  command <- stringr::str_c("exiftool ", '"', file, '"')
  
  # Use system command
  system_string <- system(command, intern = TRUE)
  
  # Split string into variable and value
  system_string_split <- stringr::str_split_fixed(system_string, ":", n = 2)
  
  # Split string
  variable <- system_string_split[, 1]
  value <- system_string_split[, 2]
  
  # Strip unneeded whitespace
  variable <- stringr::str_trim(variable)
  
  # Remove special characters first
  # value <- stringr::str_replace_all(value, "[^[:alnum:]///' ]", "")
  value <- stringr::str_trim(value)
  
  # Clean some things up
  variable <- stringr::str_replace_all(variable, " ", "_")
  variable <- stringr::str_replace_all(variable, "/", "_")
  variable <- tolower(variable)
  
  # If there are duplicated variables, append a suffix
  variable <- make.names(variable, unique = TRUE)
  variable <- stringr::str_replace_all(variable, "\\.", "_")
  
  # Make data frame
  data <- data.frame(file = file, file_basename, variable, value)
  
  # Reshape data
  data <- tidyr::spread(data, variable, value)
  
  # Return
  data
  
}
