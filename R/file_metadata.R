#' Function to extract file metadata with \code{exiftool}.
#' 
#' \code{file_metadata} uses Phil Harvey's Perl \code{exiftool} to extract a 
#' file's metadata and then formats the output into a table which can be 
#' conveniently used within R. 
#' 
#' \code{exiftool} is called as a system command and this function has not been
#' tested on other non-Debian systems. I do not know how \code{exiftool} is 
#' implemented on Windows or macOS and therefore I cannot guarantee that this 
#' function will work. 
#' 
#' \code{file_metadata} can be useful for media files and the exploration of 
#' metadata. 
#' 
#' All variables which are supported and returned by \code{exiftool} will be 
#' present. Watch out for very large numbers of variables when imputing large
#' XML or JSON documents with a large numbers of nested elements.
#' 
#' @param file A vector of files to extract metadata from. 
#' 
#' @param progress Should a progress bar be displayed? 
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \href{http://www.sno.phy.queensu.ca/~phil/exiftool/}{exiftool}
#'
#' @examples
#' 
#' \dontrun{
#' data_metadata <- file_metadata("music.mp3")
#' }
#'
#' @export
file_metadata <- function(file, progress = FALSE) {
  
  # Check for programme
  detect_exiftool()
  
  # Ensure path is expanded, sometimes in necessary and then do
  df <- file %>% 
    fs::path_expand() %>% 
    purrr::map_dfr(file_metadata_worker, .progress = progress) %>% 
    as_tibble() %>% 
    mutate(across(everything(), type.convert, as.is = TRUE)) %>% 
    mutate(
      across(
        dplyr::matches("date"), 
        lubridate::ymd_hms, tz = "UTC", truncated = 3, quiet = TRUE
      )
    )
  
  return(df)
  
}


file_metadata_worker <- function(file) {

  # Get file basename
  file_basename <- basename(file)
  
  # Escape characters for bash
  file <- gsub("$", "\\$", file, fixed = TRUE)
  file <- gsub("`", "\\`", file, fixed = TRUE)

  # Build system command
  # Watch the different types of quotes here
  command <- stringr::str_c("exiftool -json ", '"', file, '"')
  
  # Use system command
  string <- system(command, intern = TRUE)
  
  # Split string into variable and value
  df <- string %>% 
    jsonlite::fromJSON() %>% 
    mutate(across(everything(), as.character))
  
  # If there are duplicated variables, append a suffix
  names(df) <- str_to_underscore(names(df))
  names(df) <- make.names(names(df), unique = TRUE)
  
  return(df)
  
}


detect_exiftool <- function() {
  
  # Test
  text <- suppressWarnings(
    system("which exiftool", intern = TRUE, ignore.stderr = TRUE)
  )
  
  # Raise error if not installed
  if (length(text) == 0 || !grepl("exiftool", text)) {
    stop("'exiftool' system programme not detected.", call. = FALSE)
  }
  
  # No return
  
}
