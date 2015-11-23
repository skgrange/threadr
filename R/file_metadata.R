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
#' @param progress What type of progress bar should the funciton display? Default
#' is \code{"time"}, use \code{"none"} for none. 
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \link{http://www.sno.phy.queensu.ca/~phil/exiftool/}
#'
#' @examples
#' \dontrun{
#' data_metadata <- file_metadata("music.mp3")
#' }
#'
#' @export
file_metadata <- function (file, progress = "time") {
  
  # Vectorise function
  df <- plyr::ldply(file, scraper, .progress = progress)
  df
  
}


# The function which does the work
#
# No export 
scraper <- function (file) {

  # Get file basename
  file_basename <- basename(file)
  
  # Escape for bash
  file <- gsub("$", "\\$", file, fixed = TRUE)
  file <- gsub("`", "\\`", file, fixed = TRUE)

  # Build system command
  # Watch the different types of quotes here
  command <- stringr::str_c("exiftool -json ", '"', file, '"')
  
  # Use system command
  string <- system(command, intern = TRUE)
  
  # Split string into variable and value
  df <- jsonlite::fromJSON(string)
  
  # If there are duplicated variables, append a suffix
  names(df) <- threadr::str_underscore(names(df))
  # names(df) <- stringr::str_to_lower(names(df))
  names(df) <- make.names(names(df), unique = TRUE)
  
  # Return
  df
  
}
