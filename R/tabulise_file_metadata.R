#' Function to extract file metadata with \code{exiftool} and store it within a 
#' data frame. 
#' 
#' tabulise_file_metadata uses Phil Harvey's Perl \code{exiftool} to extract a 
#' file's metadata and then formats the output into a table which can be 
#' conveniently used within R. 
#' 
#' \code{exiftool} is called as a system command and this function has not been
#' tested on other non-Debian systems. I do not know how \code{exiftool} is 
#' implemented on Windows or OS X and therefore I cannot guarantee that this 
#' function will work on such systems. 
#' 
#' tabulise_file_metadata can be useful for media files and the exploration of 
#' metadata. 
#' 
#' All variables which are supported and returned by \code{exiftool} will be 
#' present. Watch out for very large numbers of variables when imputing large
#' xml or json documents with a large numbers of nested elements. 
#' 
#' @author Stuart K. Grange
#'
#' @export
#'
tabulise_file_metadata <- function (file = "music.mp3") {
  
  # Get file basename
  file.basename <- basename(file)
  
  # Escape $ in bash
  file <- gsub("$', '\\$", file, fixed = TRUE)
  # Escape ` in bash
  file <- gsub("`', '\\`", file, fixed = TRUE)
  
  # Build system command
  command <- stringr::str_c('exiftool ', '"', file, '"')
  
  # Use system command
  system.string <- system(command, intern = TRUE)
  
  # Split string into variable and value
  system.string.split <- stringr::str_split_fixed(system.string, ":", n = 2)
  
  # Split string
  variable <- system.string.split[, 1]
  value <- system.string.split[, 2]
  
  # Strip unneeded whitespace
  variable <- stringr::str_trim(variable)
  
  # Remove special characters first
  value <- stringr::str_replace_all(value, "[^[:alnum:]///' ]", "")
  value <- stringr::str_trim(value)
  
  # Clean some things up
  variable <- stringr::str_replace_all(variable, " ", ".")
  variable <- stringr::str_replace_all(variable, "/", ".")
  variable <- tolower(variable)
  
  # If there are duplicated vairables, append a suffix
  variable <- make.names(variable, unique = TRUE)
  
  # Make data frame
  data <- data.frame(file = file, file.basename, variable, value)
  
  # Reshape data
  data <- tidyr::spread(data, variable, value)
  
  # Return
  data
  
}
